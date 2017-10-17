

changerid<-function(data, anciennom, nouveaunom) {
	if(class(data)!="xtrain.multi") stop("data must be of the class xtrain.multi ")
	if(class(colonne)!="character") stop("colonne must be of the class character")
	if(class(anciennom)!="character") stop("anciennom must be of the class character")
	if(class(nouveaunom)!="character") stop("nouveaunom must be of the class character")
	if(colonne%in%names(data@resume)==FALSE) stop("colonne must be name of resume")
	ancien=as.character(anciennom)
	nouveau=as.character(nouveaunom)
	dtest=data@resume$id
	test=which(dtest%in%ancien)
	levels(data@resume$id)[levels(x)=="beta"]
	stest=data@resume$id
	cloc<-data@resume[stest,colonne]<-nouveau
	indice=data@resume[test,]$list.place

	return(test)

#	lf=paste(data@list.data[[indice]],colonne,sep="@")

#	lfd=paste(data@list.data[[indice]]$data,colonne,sep="$")
#	lfda=paste(data@list.data[[indice]]$aggr.data,colonne,sep="$")
#	lfdam=paste(data@list.data[[indice]]$med.aggr.data,colonne,sep="$")

#	data@resume=which(get(df)==anciennom)<-nouveaunom
#	get(lf)<-nouveaunom
#	get(lfd)<-nouveaunom
#	get(lfda)<-nouveaunom
#	get(lfdam)<-nouveaunom
#	return(data@resume)
}
