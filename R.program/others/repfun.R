df<-read.table("D:/ref.cr.txt")
dg<-read.table("D:/ref.gc.txt")
gc=read.table("D:/gc.txt")
cr=read.table("D:/cr.txt")
repfun <- function(X0,Y0,X,Y,i){
  DF=data.frame(y=Y0[,i],x=X0[,i])
  lm.ref.crgc=c()
  lm.ref<-loess(y~x,span=10,degree=1,data=DF,control=loess.control(surface="direct"))
  X=data.frame(x=X[,i])
  Y=Y[,i]
  lm.pred=predict(lm.ref,newdata=X)
  res_ref=lm.pred-Y
  sd=sqrt(sum(res_ref^2)/(length(res_ref)-1))
  T_test=res_ref*sqrt(length(res_ref)-1)/sd
  return(T_test)
}
I <- c(2:23)
result <- sapply(I,function(i) repfun(X0=dg,Y0=df,X=gc,Y=cr,i))
result