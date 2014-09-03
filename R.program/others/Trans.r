RawData <- read.csv('C:\\Users\\Administrator\\Desktop\\20101101-20111031_TOP3_Transactions.csv', head = TRUE)
Coust_ID=unique(RawData[,1]);
clust=c("Ready To Wear",
        "Accessories",
        "Shoes",
        "Jewelry",
        "Leather Goods",
        "Unknown",
        "Watches"
)
Out=rep(0,4)
for(ID in Coust_ID){
  
  temp=RawData[RawData$DREAM_ID==ID,];
  #temp=temp[sort(temp$Product_Type),];
  i=1;
  for(type in clust){
    temp[temp$Product_Type==type,];
    Sum_Quant=sum(temp$Quantity);
    Sum_Turnover=sum(temp$Turnover);
    Result=matrix(0,7,4);
    Result=as.data.frame(Result);
    Result[i,1]=ID;
    Result[i,2]=type;
    Result[i,3]=Sum_Quant;
    Result[i,4]= Sum_Turnover;
    names(Result)[1]="DREAM_ID";
    names(Result)[2]="Product_Type";
    names(Result)[3]="SumQuant";
    names(Result)[4]="SumTurnover";
    i=i+1;
  }
  Out=rbind(Out,Result)
}