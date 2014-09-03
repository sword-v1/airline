setwd("C:/Users/Administrator/Desktop/发动机数据")
list=c("B-2613","B-2645","B-2648","B-2671","B-5168",
       "B-5176","B-5211","B-5228","B-5312","B-5325")

for(j in 1:length(list))
{
no=list[j]
txtname=paste(no,".txt",sep="")
csvname=paste(no,".csv",sep="")

rt<-read.table(txtname,header=F,sep="\t")
rt<-as.character(rt[,1])

title<-c()
value<-c()

for( i in 1:length(rt) )  #for的速度比较慢，但我暂时没有想到其他的方法
{
  possible<-tryCatch({temp<-strsplit(substr(rt[i],1,nchar(rt[i])-1),' = ')},error=function(e){e})
  if (inherits(possible, "error")) next
  if(is.na(temp[[1]][2])==F)
  {
    title<-c(title,temp[[1]][1])
    value<-c(value,temp[[1]][2])
  }
}

namelist<-unique(title) #数据框的header

nc=length(namelist)
nl=length(title)/length(namelist)

rdt=array("",dim=c(nl,nc))
for(i in 1:length(namelist))
{
   rdt[,i]<-value[title==namelist[i]]
}

#不用for的方法，结果一样，估计会稍微快点
#rdt1<-value[title==namelist[1:nc]]
#dim(rdt1)<-c(nc,nl)
#rdt1<-t(rdt1)

rdt<-as.data.frame(rdt)
colnames(rdt)<-namelist

write.csv(rdt,csvname)

}