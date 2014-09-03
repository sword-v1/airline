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
ItemClt <- function(U,n,k,l){
  ##  数据标准化处理，构造评分矩阵
  MM <- read.csv('C:\\Users\\Administrator\\Desktop\\LVDATA\\news.csv', header = TRUE)
  MM <- as.matrix(MM)
  #U <- matrix(c(19845376, 3, 343049.99, 0, 0.00, 5, 71499.98, 0, 0.00, 1, 416000, 0, 0),1,13)
  #U <- matrix(c(21859680, 1, 320, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),1,13)
  #U <- matrix(c(55904000, 11, 53139.92, 4, 12399.99, 0, 0, 0, 0, 0, 0, 4, 65799.99),1,13)
  test <- (MM[,2]+MM[,4]+MM[,6]+MM[,8]+MM[,10]+MM[,12]<=20)  # 清洗giftbuyer数据
  MM <- MM[test,]  
  mtotal <- MM[,3]+MM[,5]+MM[,7]+MM[,9]+MM[,11]+MM[,13]
  MM <- cbind(MM,mtotal)
  MM <- MM[order(MM[,14],decreasing = TRUE),-14] 
  MM <- head(MM,n)
  MM <- rbind(MM,U)
  test1 <- which(MM[,2]>0)  # 计算每个用户对应product_type的均值
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
  MM <- scale(MM[,-1],center=TRUE)  # 数据标准化处理
  MM <- t(MM)
  Simp <- cor(t(MM))  # 采用相关相似度量计算相似度
  Simf <- apply(Simp, 1, FUN=sort, decreasing=TRUE, index.return=TRUE) 
  ########################### 取出前k-1个分值最大的USER_ID,计算预测用户的得分
  res <- lapply(Simf, FUN=function(r) return(r$ix[1:k]))
  ru <- t(as.data.frame(res[])[-1,])
  m <- apply(MM,1,mean)
  Sc <- rep(0,12)
  for (i in 1:12){
    temp <- ru[i,]
    Sc[i] <- m[i]+sum(Simp[i,temp]-m[temp])/sum(Simp[i,temp]) 
  }
  Sc <- Sc[c(2,4,6,8,10,12)]
  product.type <- c('Leather Goods','Accessories','Ready To Wear','Shoes','Watches','Jewelry')
  S <- sort(Sc, decreasing=TRUE, index.return=TRUE)
  result <- product.type[S$ix[1:l]]
  list(result=result,fscore=Sc,user.k=ru,Simp=Simp,MM=MM,ID=ID)
}
