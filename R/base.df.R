base.df<-function(x,incidence='base',type='ENDO_CIRC_STRAIN') {
if(class(x)!="xtrain.multi") stop("object must be of the class xtrain.multi ")

ind=unique(x@resume[incidence=='base'&type=='ENDO_CIRC_STRAIN'&empty==FALSE]$list.place)

mylist<-as.list(vector(length=length(ind)))

for(i in 1L:length(ind)) {
mylist[[i]]<-x@list.data[[ind[i]]]@aggr.data
}
#	dataframe=dataframe[dataframe$type==type,]
return(rbindlist(mylist))
}
