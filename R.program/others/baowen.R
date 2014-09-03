setwd("C:/Users/Administrator/Desktop/数据")
list=c("330-R04")

for(j in 1:length(list))
{
no=list[j]
txtname=paste(no,".txt",sep="")
csvname=paste(no,".csv",sep="")
title<-c()
value<-c()

rt=scan(file=txtname,what=character(),sep="\t")

rtn=substr(rt,1,nchar(rt,type = "width")-1)
temp=strsplit(rtn,' = ')
a=as.numeric(lapply(temp,length))
tmp=temp[a==2]

tmp1=unlist(tmp)
n=length(tmp1)
title=tmp1[seq(1,n,2)]
value=tmp1[seq(2,n,2)]

namelist<-unique(title) #数据框的header
nc=length(namelist)
nl=length(title)/length(namelist)

rdt=array("",dim=c(nl,nc))
# for(i in 1:length(namelist))
# {
#    rdt[,i]<-value[title==namelist[i]]
# }

#不用for的方法，结果一样，估计会稍微快点
rdt1<-value[title==namelist[1:nc]]
dim(rdt1)<-c(nc,nl)
rdt1<-t(rdt1)

rdt1<-as.data.frame(rdt1)
colnames(rdt1)<-namelist

write.csv(rdt1,csvname,row.names=F)

}

