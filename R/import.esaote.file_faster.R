#' Import a csv file from Esaote echograph
#'
#' @param file A csv file
#' @param condition The identification tag for the experiment group
#' @return A object of class 'xtrain.one'
#' @examples
#' import.esaote.file_faster('ENDO_LONG_DISPL_SUBJECT1.csv')
#' @export
import.esaote.file_faster<-function(file, condition=NA) {
brut=scan(file,what="character",fileEncoding="UTF-8",sep="\n",quiet=TRUE) # change
brut2=strsplit(brut,",",fixed=TRUE)


session=substring(file, nchar(file)-25,nchar(file)-4)
#	id=unlist(strsplit(substring(file, nchar(file)-27,nchar(file)-4),"__"))[2]
toomuchcol=vector(length=1)
nom_df=brut2[[21]]
nom_df[2]<-"Sync"
nom_df[3]<-"Time"

if(nom_df[4]=="Segment 1 (MOYANTSEP)") {
	incidence="base"
	nom=c("Frame", "Sync", "Time","MOYANTSEP_S1","MOY_ANT_S2","MOY_LAT_S3","MOY_POST_S4","MOY_INF_S5","MOY_SEP_S6")
	#strtmp=c(1,3:9)
	if(length(brut2[[21]])>9) toomuchcol<-TRUE
	else toomuchcol<-FALSE
}
else {
if(nom_df[4]=="Segment 1 (BAS SEP)") {
incidence="long"
nom=c("Frame","Sync","Time","BAS_SEP_S1","MOY_SEP_S2","APIC_SEP_S3","BAS_LAT_S4","MOY_LAT_S5","APIC_LAT_S6")
#strtmp=c(1,3:9)
if(length(brut2[[21]])>9) toomuchcol<-TRUE
else toomuchcol<-FALSE
}
else {
if(brut2[[17]][2]=="AREA") {
incidence="area"
nom=c("Frame","Sync","Time","Point_1")
#strtmp=c(1,3:4)
if(length(brut2[[21]])>4) toomuchcol<-TRUE
else toomuchcol<-FALSE
}
else {
if(brut2[[17]][2]=="VOLUME") {
incidence="volume"
nom=c("Frame","Sync","Time","Point_1")
#strtmp=c(1,3:4)
if(length(brut2[[21]])>4) toomuchcol<-TRUE
else toomuchcol<-FALSE
}

else {
if(nom_df[4]=="Segment 1 (BAS INF)") {
incidence="apic_2seg"
nom=c("Frame","Sync","Time","BAS_SEP_S1","MOY_SEP_S2","APIC_SEP_S3","BAS_LAT_S4","MOY_LAT_S5","APIC_LAT_S6")
#strtmp=c(1,3:9)
if(length(brut2[[21]])>9) toomuchcol<-TRUE
else toomuchcol<-FALSE
}

else {
if(nom_df[4]=="Point 1") {
incidence="apex"
nom=gsub(" ","_",nom_df)
#strtmp=c(1,3:11)
}
}
}
}
}
}

data_df=setDT(transpose(brut2[22:length(brut2)]))
data_df=setnames(data_df,nom)
col.n=ncol(data_df)

strtmp=c(1,3:col.n)
strtmp=names(data_df)[strtmp]
#print(col.n)
data_df[, (strtmp) := lapply(.SD,as.numeric), .SDcols=strtmp]
#strtmp=names(data_df[,!2,with=FALSE])
#str(data_df)
#print(incidence)
if(incidence!="area" & incidence!="volume") {
data_df$Mean=rowMeans(data_df[,4:(col.n),with=FALSE])
}

#########################################
#	Fonction d'incrément pour cycle		#
#########################################

count<-0
cycle=sapply(data_df$Sync,function(x) {
ifelse(x=="*",count<<-count+1,count)
})
cycle[length(cycle)]=cycle[length(cycle)-1]
#print(cycle)

data_df$cycle=cycle

n.cycle=cycle[length(cycle)-1]

empty=ifelse(n.cycle==0|toomuchcol==TRUE,TRUE,FALSE)
n.cycle=ifelse(n.cycle==0,-1,n.cycle)
id=brut2[[9]][2]
data_df$id=id

data_df=data_df[data_df$cycle!=0]


#if(empty==FALSE) {
data_df$Time.sync=data_df$Time-data_df$Time[1]
#}

type=brut2[[17]][2]

data_df$type=type
data_df$incidence=incidence
data_df$session=session

t3=unlist(strsplit(type,"_"))[length(strsplit(type,"_"))]
data_df$t3=t3
##### Fonction pour calculer la correlation entre les segments selon l'incidence
corr.funct <- function(x, type) {
switch(type,
base = lapply(split(x, x$cycle),function(x) cor(x[,4:9,with=FALSE])),
long = lapply(split(x, x$cycle),function(x) cor(x[,4:9,with=FALSE])),
area = lapply(split(x, x$cycle),function(x) cor(x[,4,with=FALSE],x[,4,with=FALSE])),
volume= lapply(split(x, x$cycle),function(x) cor(x[,4,with=FALSE],x[,4,with=FALSE])),
apic_2seg=lapply(split(x, x$cycle),function(x) cor(x[,4:9,with=FALSE])),
apex = lapply(split(x, x$cycle),function(x) cor(x[,4:(8+3),with=FALSE] ) )
)
}
#	print(yi)

data_df$condition=condition
data_df$Time.sync=data_df[,Time.sync-min(Time.sync),by=cycle]$V1

#	print(ci)

if(empty==FALSE) {
maxtime=data_df[,max(Time.sync),by=cycle]$V1
ci=corr.funct(data_df,incidence)
c.test=lapply(ci,function(x) sum(x<.40)/(sqrt(length(x))*2))
hr=round(60/(mean(maxtime)/1000))
}
else {
maxtime=0
ci=list(NA)
c.test=list(NA)
hr=0
}

#	if(n.cycle!=-1) condition=rep(condition,n.cycle)
condition=condition


#	systole=vector()
if(incidence=="area"||incidence=="volume") {
systole=data_df[, .SD[which.min(.SD$'Point_1')],by=cycle]$Time.sync
#str(data_df)
if(0%in%systole) {
message("il y a une durée de systole égale à 0")
systole=NA
n.cycle=-1
#print(systole[i])
}
} else {
if(empty==FALSE) systole=rep(NA,n.cycle)
else {
systole=NA
}
}

return(new("xtrain.one",
data=data_df,
id=id,
condition=condition,
empty=empty,
incidence=incidence,
session=session,
type=type,
t3=t3,
maxtime=maxtime,
hr=hr,
corr.data=ci,
n.cycle=n.cycle,
systole=systole,
corr.test=c.test))

}
