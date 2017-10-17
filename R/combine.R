combine<-function(x, y){
z=new("xtrain.multi")
if(class(x)!="xtrain.multi"|class(y)!="xtrain.multi") stop("object must be of the class xtrain.multi ")
y@resume$list.place=y@resume$list.place+max(x@resume$list.place)

z@resume=rbind(x@resume,y@resume)
z@list.data=c(x@list.data,y@list.data)

return(z)
}
