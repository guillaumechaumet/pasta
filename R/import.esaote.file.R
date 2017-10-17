import.esaote.file <- function(file, condition="NA", apex.n=8) {
	library(data.table)
	brut=scan(file,what="character",fileEncoding="UTF-8",sep="\n") # change

	e_long=c("ENDO_LONG_DISPL","ENDO_LONG_STRAIN","ENDO_LONG_STRAIN_RATE", "ENDO_LONG_VEL","ENDO_TRANSV_DISPL","ENDO_TRANSV_VEL","ENDO_VP_VEL","EPI_LONG_DISPL","EPI_LONG_STRAIN","EPI_LONG_STRAIN_RATE","EPI_LONG_VEL","EPI_TRANSV_DISPL","EPI_TRANSV_VEL", "EPI_VP_VEL","RADIAL_STRAIN","RADIAL_STRAIN_RATE")
	e_base=c("ENDO_CIRC_STRAIN","ENDO_CIRC_STRAIN_RATE","ENDO_RADIAL_DISPL","ENDO_RADIAL_VEL","ENDO_ROT_DISPL","ENDO_ROT_VEL","ENDO_VP_VEL","EPI_CIRC_STRAIN","EPI_CIRC_STRAIN_RATE","EPI_RADIAL_DISPL","EPI_RADIAL_VEL","EPI_ROT_DISPL","EPI_ROT_VEL","EPI_VP_VEL","RADIAL_STRAIN","RADIAL_STRAIN_RATE")

	session=substring(file, nchar(file)-25,nchar(file)-4)
	#	id=unlist(strsplit(substring(file, nchar(file)-27,nchar(file)-4),"__"))[2]

	filt=brut[22:length(brut)]

	filt=strsplit(filt,",")
	if(strsplit(brut[21],",")[[1]][4]=="Segment 1 (MOYANTSEP)") {
		incidence="base"
		col.n=9
		nom=c("Frame", "Sync", "Time","MOYANTSEP_S1","MOY_ANT_S2","MOY_LAT_S3","MOY_POST_S4","MOY_INF_S5","MOY_SEP_S6")
	}
	else {
		if(strsplit(brut[21],",")[[1]][4]=="Segment 1 (BAS SEP)") {
			incidence="long"
			col.n=9
			nom=c("Frame","Sync","Time","BAS_SEP_S1","MOY_SEP_S2","APIC_SEP_S3","BAS_LAT_S4","MOY_LAT_S5","APIC_LAT_S6")
		}
		else {
			if(strsplit(brut[17],",")[[1]][2]=="AREA") {
				incidence="area"
				col.n=4
				nom=c("Frame","Sync","Time","Point_1")
			}
			else {
				if(strsplit(brut[17],",")[[1]][2]=="VOLUME") {
					incidence="volume"
					col.n=4
					nom=c("Frame","Sync","Time","Point_1")
				}
				else {
					if(strsplit(brut[21],",")[[1]][4]=="Segment 1 (BAS INF)") {
						incidence="apic_2seg"
						col.n=9
						nom=c("Frame","Sync","Time","BAS_SEP_S1","MOY_SEP_S2","APIC_SEP_S3","BAS_LAT_S4","MOY_LAT_S5","APIC_LAT_S6")
					}
					else {
						if(strsplit(brut[21],",")[[1]][4]=="Point 1") {
							incidence="apex"
							col.n=length(strsplit(brut[21],",")[[1]])
							v.n=(4:col.n)-3
							for(i in 1:(col.n-3))
							v.n[i]<-paste("Point",i,sep="_")
							nom=c("Frame","Sync","Time",v.n)
						}


					}
				}



			}
		}
	}
		#########################################
		#	Fonction d'incrément pour cycle		#
		#########################################
	increment.func<-function(x, element) {
		produit=rep(0,length.out=length(x))
		for(i in 2L:(length(x))) {
			if(x[i-1]==element)
			produit[(i-1):length(x)]<-produit[(i-1):length(x)]+1
		}
		return(produit)
	}

	Sync=unlist(lapply(filt, function(x) x[2]))

	n.cycle=sum(Sync=="*")-1

	vec=as.numeric(unlist(filt)[1:length(unlist(filt))])
	yi=matrix(vec,ncol=col.n,nrow=length(filt),byrow=T)
	yi=data.frame(yi)
#	return(yi)

	if(incidence=="apex") {

		xxx=t(apply(yi[,4:col.n],1,function(x) spline(x,n=apex.n)$y))
		yi<-yi[,1:3]

		yi<-cbind(yi,xxx)
		col.n=apex.n
		v.n=(4:col.n)-3
		for(i in 1:(col.n))
		v.n[i]<-paste("Point",i,sep="_")
		nom=c("Frame","Sync","Time",v.n)
	}

	colnames(yi)<-nom

	if(incidence!="area" & incidence!="volume") {
		yi$Mean=rowMeans(yi[,4:(col.n)])
	}

	yi$id=strsplit(brut[9],",")[[1]][2]
	yi$Sync<-Sync
#	if(yi$Time[1L]==0&yi$Sync[2L]!="*") {
#		yi$Time.Sync=yi$Time
#		yi$Sync[1]<-"*"
#		debut=1L
#	} else {
	###### BUG du "*" le début c'est le premier "*" sauf quand le fichier est vide
	yi$Time.Sync=yi$Time-yi$Time[2]
	if(n.cycle!=-1) debut=which(yi$Sync=="*")[1]
	else debut=1

#	}


	yi$type=strsplit(brut[17],",")[[1]][2]
	yi$incidence=incidence
	yi$session=session
#	yi$condition=rep(condition,nrow(yi))

#	return(yi) # DEBUG

	yi$t3=unlist(lapply(strsplit(as.character(yi$type),"_"),function(x) x[max(length(x))]))

	moy.min=c(yi$Time[which(yi$Mean==min(yi$Mean),arr.ind=TRUE)],min(yi$Mean))
	yi$cycle=increment.func(yi$Sync,"*")
	##### Fonction pour calculer la correlation entre les segments selon l'incidence
	corr.funct <- function(x, type) {
	  switch(type,
	  base = lapply(split(x, x$cycle),function(x) cor(x[,4:9])),
	  long = lapply(split(x, x$cycle),function(x) cor(x[,4:9])),
	  area = lapply(split(x, x$cycle),function(x) cor(x[,4],x[,4])),
	 volume= lapply(split(x, x$cycle),function(x) cor(x[,4],x[,4])),
	  apic_2seg=lapply(split(x, x$cycle),function(x) cor(x[,4:9])),
	  apex = lapply(split(x, x$cycle),function(x) cor(x[,4:(apex.n+3)] ) )
	  )
	 }
#	print(yi)



	systole=vector()
	zi=yi[debut:nrow(yi),]
	zi$condition=rep(condition,nrow(zi))
	ci=corr.funct(zi,incidence)
	c.test=lapply(ci,function(x) sum(x<.40)/(sqrt(length(x))*2))


#	print(ci)
	maxtime=vector()
	Time.Sync=vector()
	if(n.cycle!=-1) {
		for(i in 1L:n.cycle) {
			maxtime[i]=max((subset(zi,cycle==i)$Time.Sync)-min(subset(zi,cycle==i)$Time.Sync)) # calcule le maxtime

			Time.Sync=c(Time.Sync,subset(zi,cycle==i)$Time.Sync-min(subset(zi,cycle==i)$Time.Sync)) # reinitialise le Temps pour chaque cycle
		}
	}
	else maxtime=NA

#	if(n.cycle!=-1) condition=rep(condition,n.cycle)
	condition=condition
	hr=round(60/(mean(maxtime)/1000))
	systole=vector()
	if(incidence=="area"||incidence=="volume") {
		for(i in 1L:n.cycle) {
			zi$Time.Sync=Time.Sync
			systole[i]=subset(zi,cycle==i)$Time.Sync[which.min(subset(zi,cycle==i)$Point_1)]-min(subset(zi,cycle==i)$Time.Sync)
			if(systole[i]==0) {
				message("il y a une durée de systole égale à 0")
				systole[i]=NA
				n.cycle=-1
			}
		}
	} else
	{
		if(n.cycle!=-1) {
			for(i in 1L:n.cycle) {
				systole[i]=NA
			}
			zi$Time.Sync=Time.Sync

		}
	}

	return(new("xtrain.one",data=zi,id=yi$id[1L],condition=condition,incidence=yi$incidence[1L],session=session,type=yi$type[1L],t3=yi$t3[1L],apex.n=apex.n,maxtime=maxtime,hr=hr,corr.data=ci,n.cycle=n.cycle,systole=systole,corr.test=c.test))
	# return(filt)

}
