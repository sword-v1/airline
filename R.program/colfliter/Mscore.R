## 导入数据库中的数据LVDATA，该数据集已经剔除了quantity,turnover<0以及sku_name为空值的情况
library('ORE')
ore.connect('rquser','orcl1','192.168.6.47','rquser',all=T)
lvdata <- ore.pull(LVDATA)  # 读取数据，对相同id的数据进行合并整理
user.id <- unique(lvdata$DREAM_ID)
general.type <- unique(lvdata$PRODUCT_TYPE)
n <- length(user.id)
news <- matrix(0,n,13,dimnames = list(c(1:n),c('user.id','L1','L2','A1','A2','R1','R2','S1','S2','W1','W2','J1','J2')))
i <- 1
for (id in user.id){ 
  news[i,1] <- id
  temp <- lvdata[lvdata$DREAM_ID==id,]
  j <- 2
   for (type in general.type){
     temp1 <- temp[temp$PRODUCT_TYPE==type,]
     news[i,j] <- sum(temp1$QUANTITY)
     news[i,j+1] <- sum(temp1$TURNOVER)
     j <- j+2
   }
  i <- i+1
}
write.csv(news,'C:\\Users\\Administrator\\Desktop\\news.csv')  # 写为csv文件

##  数据标准化处理，构造评分矩阵
MM <- read.csv('C:\\Users\\Administrator\\Desktop\\LVDATA\\news.csv', header = TRUE)
MM <- as.matrix(MM)
test <- (MM[,2]+MM[,4]+MM[,6]+MM[,8]+MM[,10]+MM[,12]<=20)  # 清洗giftbuyer数据
MM <- MM[test,]  # 计算每个人对应product_type的均值
mtotal <- MM[,3]+MM[,5]+MM[,7]+MM[,9]+MM[,11]+MM[,13]
MM.sub <- cbind(MM,mtotal)
MM <- MM.sub[order(MM.sub[,14],decreasing = TRUE),-14]  # 按成交金额排序
test1 <- which(MM[,2]>0)
test2 <- which(MM[,4]>0)
test3 <- which(MM[,6]>0)
test4 <- which(MM[,8]>0)
test5 <- which(MM[,10]>0)
test6 <- which(MM[,12]>0)
MM[test1,3] <- MM[test1,3]/MM[test1,2]
MM[test2,5] <- MM[test2,5]/MM[test2,4]
MM[test3,7] <- MM[test3,7]/MM[test3,6]
MM[test4,9] <- MM[test4,9]/MM[test4,8]
MM[test5,11] <- MM[test5,11]/MM[test5,10]
MM[test6,13] <- MM[test6,13]/MM[test6,12]
USER_ID <- MM[,1]
MM <- cbind(USER_ID,scale(MM[,-1],center=FALSE)) # 数据标准化处理

## 计算相似性度量
N <- 2000  # 选取成交金额最大的前N个客户进行协同过滤分析
MM <- head(MM[,-1],N)
ID <- head(USER_ID,N)
T <- matrix(0,N,N)  # 采用余弦相似度量计算相似度
for (i in 1:N)
  for (j in 1:N){
    T[i,j] <- sqrt(crossprod(MM[i,])*crossprod(MM[j,]))
    }
Sim <- tcrossprod(MM)/T
Sim <- apply(Sim, 1, FUN=sort, decreasing=TRUE, index.return=TRUE) 
k <- 6
# 取出前5个分值最大的subject
res <- lapply(Sim, FUN=function(r)return(ID[r$ix[1:k]]))
write.table(paste(ID, res, sep=':'), file='C:\\Users\\Administrator\\Desktop\\result.dat', quote=FALSE, row.name=FALSE, col.name=FALSE)