#' Import multiple csv files from Esaote echograph
#'
#' @param path A directory where all the files are (character)
#' @param condition The identification tag for the experiment group (character)
#' @param apex The number of points by default for the apex (numeric)
#' @param echotype The type of echograph. Currently, only the esaote output are analyzed (character)
#' @param multicore If there is parallel importation or not (Logical)
#' @param cores The number of cores for parallel importation option. By default, 'ALL' used every cores.
#' @return An object of class 'xtrain.multi'
#' @examples
#' import.esaote.file_faster('ENDO_LONG_DISPL_SUBJECT1.csv')
#' @export
multi.import.xtrain<-function(path=".",condition=NA,apex.n=8, echotype="esaote",multicore=FALSE,cores='ALL') {

#	importe plusieurs fichiers à la fois
# library(data.table)
ncores<-detectCores(all.tests = FALSE, logical = TRUE)
if(cores=='ALL'){
	options(mc.cores=ncores)
}

filetype<-'.csv'

library(data.table)


####################################################################
##### Je change en utilisant list.files
file.split=list.files(path=path,pattern=filetype,full.names=TRUE,recursive=TRUE)
####################################################################
message(length(file.split),' files detected \n')

Sys.setlocale("LC_TIME","en_US.UTF-8")
#	toto=vector("list",length(file.split))

########################## importe dans une liste "toto" chacun des fichiers xtrain.one ################
if(echotype=="esaote") {
import.file<-import.esaote.file_faster
}
else {
if(echotype=="gee") {
import.file<-import.gee.file
}
}

if (multicore==TRUE){
	toto=mcmapply(file.split,FUN=import.file,condition=condition)
} else {
	toto=mapply(file.split,FUN=import.file,condition=condition)
}



min.n=vector("list",length(file.split))

count=0

resume=data.table(row.numb=1L:length(unlist(lapply(toto,function(x) rep(x@id,abs(x@n.cycle))))),
id=unlist(lapply(toto,function(x) rep(x@id,abs(x@n.cycle)))),
list.place=unlist(lapply(toto,function(x) {
count<<-count+1
return(rep(count,abs(x@n.cycle)))
})), # création d'une colonne qui stocke la place dans la liste
session=unlist(lapply(toto,function(x) rep(x@session,abs(x@n.cycle)))),
condition=unlist(lapply(toto,function(x) rep(x@condition,abs(x@n.cycle)))),
incidence=unlist(lapply(toto,function(x) rep(x@incidence,abs(x@n.cycle)))),
empty=unlist(lapply(toto,function(x) rep(ifelse(x@n.cycle==-1,TRUE,FALSE),abs(x@n.cycle)))),
type=unlist(lapply(toto,function(x) rep(x@type,abs(x@n.cycle)))),
t3=unlist(lapply(toto,function(x) rep(x@t3,abs(x@n.cycle)))),
maxtime=unlist(lapply(toto,function(x) x@maxtime)),
hr=unlist(lapply(toto,function(x) rep(x@hr,abs(x@n.cycle)))),
n.cycle=unlist(lapply(toto,function(x) rep(x@n.cycle,abs(x@n.cycle)))),
cycle=unlist(lapply(toto,function(x) 1L:abs(x@n.cycle))),
systole=unlist(lapply(toto,function(x) x@systole)),
corr.test=unlist(lapply(toto,function(x) unlist(x@corr.test))))

##### calcul systole ########
resume$session.cycle=paste(resume$session,resume$cycle,sep="_")
area.session=resume$session.cycle[which(resume$empty==FALSE & (resume$incidence=="area" ))]
volume.session=resume$session.cycle[which(resume$empty==FALSE & (resume$incidence=="volume" ))]


systole=vector("numeric",length=nrow(resume))

##### creer un vecteur indice de position pour les fichiers area ######
##### remplit le vecteur systole avec les systoles area ########
resume[,systole:=ifelse(is.na(systole),na.omit(.SD$systole),systole),by=session.cycle]


##### integre dans le fichier resume les systoles
#resume$systole<-unlist(systole)
#browser()

#### calcule les proportions "systole/duree du cycle" pour l'apex ######
resume[empty==FALSE,systole_th2:=(systole/maxtime)*maxtime,by=session.cycle]

resume[incidence!="apex"&empty==FALSE,prop.vec:=mean(systole/maxtime),by=session.cycle]


resume[empty==FALSE,systole.th:=prop.vec*maxtime]
resume[,prop_mean:=mean(prop.vec,na.rm=TRUE),by=c('id','cycle')]

resume[incidence=='apex'&is.na(systole.th),systole.th:=prop_mean*maxtime]


# prop.df=aggregate(resume$prop.vec, list(id=resume$id,cycle=resume$cycle),mean,na.rm=TRUE)
# 	#print(prop.df)
# resume=merge(resume,prop.df,by.x=c("session_global","cycle"),by.y=c('id','cycle'),all.x=TRUE,sort=FALSE)
# resume=resume[order(resume$row.numb),]
#
# resume$systole.th=resume$x*resume$maxtime # calcul la systole théorique
resume[resume$incidence=="apex",]$systole<-resume[resume$incidence=="apex",]$systole_th2

resume$session.cycle=as.factor(resume$session.cycle)

resume2=subset(resume,resume$cycle>0&resume$empty==FALSE)
#print(resume2)
#print(resume3)


	for(i in resume2[!is.na(systole)&!is.na(systole.th)]$list.place) {
	#toto[[i]]@systole<-as.vector(toto3[i,2L:((toto[[i]]@n.cycle)+1)])
	systole.df=vector(mode="numeric",
	length=length(toto[[i]]@data$Time.sync))
	#print("resume@systole")
	#print(resume$systole)
	test=toto[[i]]@data$Time.sync
	#print(toto[[i]])
	#			negatif=vector(mode="numeric",length(toto[[i]]@data$Time.Sync))
	#			positif=vector(mode="numeric",length(toto[[i]]@data$Time.Sync))

	moment=vector(mode="character",length(toto[[i]]@data$Time.sync))
	fin=0
	cut.time=vector(length=length(toto[[i]]@data$Time.sync),mode="numeric") # vector de proportion temporelle

	for(j in 1L:toto[[i]]@n.cycle) {
	# print(toto[[i]]@session)
	# print(toto[[i]]@type)
	#browser()
	#print(with(toto2, toto2[list.place == i & cycle == j, ]$systole))
	if(is.na(with(resume2, resume2[list.place == i & cycle == j, ]$systole))) {
		toto[[i]]@systole[j]<-with(resume2, resume2[list.place == i & cycle == j, ]$systole.th)
	} else {
		toto[[i]]@systole[j]<-with(resume2, resume2[list.place == i & cycle == j, ]$systole)
	}

	a=which(toto[[i]]@data$cycle==j)
	systole.df[a]=toto[[i]]@systole[[j]]

	test[a]=test[a]-systole.df[a]

	moment[a]=ifelse(test[a]>=0,"diastole","systole") ###### creation d'un vecteur qui segmente le temps

	negatif=which(test[a]<0)
	positif=which(test[a]>=0)
	cut.time[(negatif+fin)]=cut(test[(negatif+fin)],breaks=6,include.lowest=TRUE)
	cut.time[(positif+fin)]=cut(test[(positif+fin)],breaks=10,include.lowest=TRUE)
	fin=fin+max(positif)


	toto[[i]]@data$systole<-systole.df
	toto[[i]]@data$test<-test

	toto[[i]]@data$moment<-moment
	toto[[i]]@data$cut.time<-paste(moment,cut.time,sep="_")
	#print(str(toto[[i]]@data))

	############## FONCTION D'AGGREGATION ###################
	aggr.strain<-function(data,type,byf,FUN) {
		#data=as.data.table(data)

		switch(type,
		volume=data[,.("Point_1"=FUN(Point_1,na.rm=T)),
		by=byf],
		area=data[,.("Point_1"=FUN(Point_1,na.rm=T)),
		by=byf],
		base=data[,.("MOYANTSEP_S1"=FUN(MOYANTSEP_S1,na.rm=TRUE),"MOY_ANT_S2"=FUN(MOY_ANT_S2,na.rm=TRUE),"MOY_LAT_S3"=FUN(MOY_LAT_S3,na.rm=TRUE),"MOY_POST_S4"=FUN(MOY_POST_S4,na.rm=TRUE),"MOY_INF_S5"=FUN(MOY_INF_S5,na.rm=TRUE),"MOY_SEP_S6"=FUN(MOY_SEP_S6,na.rm=TRUE),"Mean"=FUN(Mean,na.rm=TRUE)),
		by=byf],
		long=data[,.("BAS_SEP_S1"=FUN(BAS_SEP_S1,na.rm=TRUE),"MOY_SEP_S2"=FUN(MOY_SEP_S2,na.rm=TRUE),"APIC_SEP_S3"=FUN(APIC_SEP_S3,na.rm=TRUE),"BAS_LAT_S4"=FUN(BAS_LAT_S4,na.rm=TRUE),"MOY_LAT_S5"=FUN(MOY_LAT_S5,na.rm=TRUE),"APIC_LAT_S6"=FUN(APIC_LAT_S6,na.rm=TRUE),"Mean"=FUN(Mean,na.rm=TRUE)),
		by=byf],
		apic_2seg=data[,.("BAS_SEP_S1"=FUN(BAS_SEP_S1,na.rm=TRUE),"MOY_SEP_S2"=FUN(MOY_SEP_S2,na.rm=TRUE),"APIC_SEP_S3"=FUN(APIC_SEP_S3,na.rm=TRUE),"BAS_LAT_S4"=FUN(BAS_LAT_S4,na.rm=TRUE),"MOY_LAT_S5"=FUN(MOY_LAT_S5,na.rm=TRUE),"APIC_LAT_S6"=FUN(APIC_LAT_S6,na.rm=TRUE),"Mean"=FUN(Mean,na.rm=TRUE)),
		by=byf],
		apex=data[,.("Point_1"=FUN(Point_1,na.rm=T),"Point_2"=FUN(Point_2,na.rm=T),"Point_3"=FUN(Point_3,na.rm=T),"Point_4"=FUN(Point_4,na.rm=T),"Point_5"=FUN(Point_5,na.rm=T),"Point_6"=FUN(Point_6,na.rm=T),"Point_7"=FUN(Point_7,na.rm=T),"Point_8"=FUN(Point_8,na.rm=T)),
		by=byf]
		)

		}
		########################################################

		}

		toto[[i]]@aggr.data=aggr.strain(data=toto[[i]]@data,
		type=toto[[i]]@incidence,
		byf=c('id','incidence','type','cycle','condition','session','cut.time'),
		FUN=mean)

		#toto[[i]]@aggr.data$cut.time=factor(toto[[i]]@data$cut.time,levels=c("systole_1", "systole_2", "systole_3", "systole_4", "systole_5", "systole_6","diastole_1", "diastole_2", "diastole_3", "diastole_4", "diastole_5", "diastole_6", "diastole_7", "diastole_8" ,"diastole_9","diastole_10"))
		toto[[i]]@med.aggr.data<-aggr.strain(data=toto[[i]]@data,
		type=toto[[i]]@incidence,
		byf=c('id','incidence','type','condition','session','cut.time'),
		#session=toto[[i]]@data$session,
		#	type=toto[[i]]@data$type,
		#	condition=toto[[i]]@data$condition,
		#	cut.time=toto[[i]]@data$cut.time),
		#type=toto[[i]]@incidence,
		#apex.n=toto[[i]]@apex.n,
		FUN=mean)

		#toto[[i]]@med.aggr.data<-toto[[i]]@med.aggr.data[order(toto[[i]]@med.aggr.data$cut.time),]

		}

	return(new("xtrain.multi",resume=resume,list.data=toto))
}
