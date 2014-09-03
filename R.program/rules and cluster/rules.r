#购物篮分析，关联规则。
library(arules)
library(Matrix)
library(lattice)

DataFrame=read.csv("C:\\Users\\wuhujun\\Desktop\\lvdata.csv",head=TRUE);

DataFrame<-DataFrame[,c(1,2,5,8,11,14,17)];
a_list=c("");
for(i in 1:length(DataFrame[,1])){
    Mark<-c();
   for( j in 2:length(DataFrame[i,])){
     if(DataFrame[i,j]!=0)
	   Mark<-c(Mark,paste("Type",as.character(j-1),sep="*"));
	   Mark<-unique(Mark);
   }
   if(length(Mark)[1]>=2)
   {
    Mark<-list(Mark);
    a_list<-c(a_list,Mark);
   }
}

a_list[1]=NULL;
trans <- as(a_list, "transactions");
rules <- apriori(trans,  parameter = list(supp = 0.1,  conf = 0.5, target="rules"))
inspect(rules)


#清除数据中的异常记录。
#DataFrame=DataFrame[!is.null(DataFrame$Product_Type)&&(DataFrame$Quantity<3)&&(DataFrame$Quantity>=1), ] ;
#所有购物者ID库。
#Cosu_ID <- unique(DataFrame$DREAM_ID);
#所有商品类别库。
#Type_ID <- unique(DataFrame$SKU_Name);

