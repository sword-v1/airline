# 基于某品牌销售记录协同过滤推荐算法案例实现
# Args: 
# U：预测用户的购买记录向量（包括用户id）  
# n：用户数量——按quantity排序取前n个数据  
# k：最近邻居的数量
# l：推荐项目的个数
# Returns:
# result： 协同过滤推荐结果
# fscore： 项目的预测得分
# user.k： 最近k个邻居的id 
#   Simp： (n+1)*(n+1)维相似性度量矩阵
#     MM： 标准化的用户数据
#     ID： 用于计算相似性矩阵的用户id
UserClt <- function(U,n,k,l){
##  数据标准化处理，构造评分矩阵
MM <- read.csv('C:\\Users\\Administrator\\Desktop\\LVDATA\\news.csv', header = TRUE)
MM <- as.matrix(MM)
#U <- matrix(c(19845376, 3, 343049.99, 0, 0.00, 5, 71499.98, 0, 0.00, 1, 416000, 0, 0),1,13)
#U <- matrix(c(21859680, 1, 320, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),1,13)
#U <- matrix(c(55904000, 11, 53139.92, 4, 12399.99, 0, 0, 0, 0, 0, 0, 4, 65799.99),1,13)
test <- (MM[,2]+MM[,4]+MM[,6]+MM[,8]+MM[,10]+MM[,12]<=20)  # 清洗giftbuyer数据
MM <- MM[test,]  # 计算每个人对应product_type的均值
mtotal <- MM[,3]+MM[,5]+MM[,7]+MM[,9]+MM[,11]+MM[,13]
MM <- cbind(MM,mtotal)
MM <- MM[order(MM[,14],decreasing = TRUE),-14] 
MM <- head(MM,n)
MM <- rbind(MM,U)
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
ID <- MM[,1]
MM <- scale(MM[,-1],center=TRUE)# 数据标准化处理
# Simp <- cor(t(MM))
T <- matrix(0,n+1,n+1)  # 采用余弦相似度量计算相似度
for (i in 1:(n+1))
  for (j in 1:(n+1)){
    T[i,j] <- sqrt(crossprod(MM[i,])*crossprod(MM[j,]))
  }
Simp <- tcrossprod(MM)/T
Simf <- apply(Simp, 1, FUN=sort, decreasing=TRUE, index.return=TRUE) 
# 取出前50个分值最大的USER_ID,计算预测用户的得分
res <- lapply(Simf, FUN=function(r) return(ID[r$ix[1:k]]))
ru <- res[[n+1]]
m <- mean(MM[n+1,])
t <- rep(0,k-1)
sim <- rep(0, k-1)
sc <- matrix(0,k-1,12)
for (j in 2:k){
  t[j-1] <- which(ID==ru[j])
  sim[j-1] <- Simp[t[j-1],n+1]
  sc[j-1,]<- MM[t[j-1],]
}
Sc <- rep(0,12)
for (i in 1:12){
  Sc[i] <- m+sum(sim*(sc[,i]-apply(sc,1,mean)))/sum(sim) 
}
Sc <- Sc[c(2,4,6,8,10,12)]
product.type <- c('Leather Goods','Accessories','Ready To Wear','Shoes','Watches','Jewelry')
S <- sort(Sc, decreasing=TRUE, index.return=TRUE)
result <- product.type[S$ix[1:l]]
list(result=result,fscore=Sc,user.k=ru,Simp=Simp,MM=MM,ID=ID)
}


#################################
#function(u,MM=MM_2000){
 # if which(MM[, 1] == u[, 1])==0
  #  u <- u
  #else
  #  u <- cbind(u[1], t(u[-1]+MM[MM[, 1]==u[, 1], -1]))
#}

####################

# MM <- read.csv('C:\\Users\\Administrator\\Desktop\\LVDATA\\news.csv', header = TRUE)
# MM <- as.matrix(MM)
# t1 <- MM[MM[,1]==46343136,]
# t2 <- MM[MM[,1]==18247216,]
# t3 <- MM[MM[,1]==58559616,]
# t4 <- MM[MM[,1]==55904000,]
# t5 <- MM[MM[,1]==22973968,]
# t6 <- MM[MM[,1]==22186016,]
# sim1 <- rbind(t1,t2,t3,t4,t5,t6)
# s1 <- MM[MM[,1]==19845376,]
# s2 <- MM[MM[,1]==19486400,]
# s3 <- MM[MM[,1]==22973008,]
# s4 <- MM[MM[,1]==32162640,]
# s5 <- MM[MM[,1]==58551936,]
# s6 <- MM[MM[,1]==48017440,]
# sim2 <- rbind(s1,s2,s3,s4,s5,s6)
# write.csv(sim1,'c:\\Users\\Administrator\\Desktop\\sim1.csv')
# write.csv(sim2,'c:\\Users\\Administrator\\Desktop\\sim2.csv')