
base.corr<-function(x,type="base") {
	if(class(x)!="xtrain.multi") stop("object must be of the class xtrain.multi ")
	select=which(x@resume$empty==FALSE&x@resume$type==type&x@resume$t3=="DISPL")

	target=x@list.data[select]

	add=lapply(target, function(x) x@corr.data)

	add1=add[[1L]]
	for(i in 2L:length(add)) {
	add1=add1+add[[i]]
	}
	add1=add1/length(add)
	diff=lapply(add, function(x) (x-add1)^2)
	diff1=diff[[1L]]
	for(i in 2L:length(diff)) {
		diff1=diff1+diff[[i]]
	}
	diff1=diff1/length(diff)
	if(type=="base") {
		calque=matrix(data=NA,nrow=6,ncol=6)
		calque[2,1]<-1
		calque[3,2]<-1
		calque[4,3]<-1
		calque[5,4]<-1
		calque[6,5]<-1
		test=lapply(add, function(x) x[which(calque==1)]<add1[which(calque==1)]-2*diff1[which(calque==1)])

	}
	if(type=="apex") {
		calque=matrix(data=NA,nrow=9,ncol=9)
		calque[2,1]<-1
		calque[3,2]<-1
		calque[4,3]<-1
		calque[5,4]<-1
		calque[6,5]<-1
		calque[7,6]<-1
		calque[8,7]<-1
		calque[9,8]<-1



		test=lapply(add, function(x) x[which(calque==1)]<add1[which(calque==1)]-2*diff1[which(calque==1)])

	}
	if(type=="long") {
		calque=matrix(data=NA,nrow=6,ncol=6)
		calque[2,1]<-1
		calque[3,2]<-1
		calque[4,3]<-1
		calque[5,4]<-1
		calque[6,5]<-1
		test=lapply(add, function(x) x[which(calque==1)]<add1[which(calque==1)]-2*diff1[which(calque==1)])

	}
	test=lapply(test, function(x) sum(x))
	iden=lapply(target, function(x) paste(x@type, x@subject, x@condition, x@id))

	for(i in 1L:length(test)) {
		names(test[[i]])<-iden[[i]]
	}
	return(list(raw=test,mean=add1,sd=diff1))

}
