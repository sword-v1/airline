setwd("C:\\Users\\Administrator\\Desktop\\LVDATA\\原始数据")
library(sqldf)

###  scan读入数据
t1 <- system.time(
scan(file = "20101101-20111031_TOP3_Transactions.csv",what=list(DREAM_ID = "", 
           Transaction_Date = "", Product_Type = "", SKU_Name = "", Item_Gender = "",
           Transaction_Channel = "", Transaction_Channel = "",Quantity  = 0, Turnover = 0),
           skip=1,sep=','))

### read.csv读入数据
t2 <- system.time(
  read.csv(file = "20101101-20111031_TOP3_Transactions.csv",header = TRUE))

t <- t2 - t1 


### 逐行读取数据
con <- file("20101101-20111031_TOP3_Transactions.csv", "r")
line=readLines(con,n=1)
while( length(line) != 0 ) {
  print(line)
  line=readLines(con,n=1)
}
close(con)

## sqldf 读取数据
f <-  file("20101101-20111031_TOP3_Transactions.csv", open = "r")
dt <- sqldf("select * from f", file.format = list(header = T, row.names = F))


