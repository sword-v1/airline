## 5.1.1 主成分分析及因子分析
w <- read.csv("C:\\Users\\Administrator\\Desktop\\LA.Neighborhoods.csv", head = TRUE)
w$density <- w$Population/w$Area
w <- w[,-c(12:15)]
a = eigen(cor(scale((w[-1]))))
(cca = (a$va)/sum(a$va))
(ca = cumsum(a$va)/sum(a$va))
plot(1:11, a$va, type = "o", pch = 17, col = 4, main = "ScreePlot", xlab = "Component Number", ylab = "Eigen Value")
par(mfrow = c(1,2))
plot(1:11, a$va, type = "o", pch = 17, col = 4, main = "ScreePlot", xlab = "Component Number", ylab = "Eigen Value")
plot(1:11, ca, type = "o", pch = 17, col = 4, main = "Cumulative Contribution", xlab = "Component Number", ylab = "Cumulative Contribution")

## 5.1.2分层聚类
w <- read.csv("C:\\Users\\Administrator\\Desktop\\LA.Neighborhoods.csv", head = TRUE)
w$density <- w$Population/w$Area
u <- w[,-c(1,2,5,6,11,16)]
hh <- hclust(dist(scale(u[,-1])),"complete")
plot(hh,labels=u[,1], cex=0.6)
id = identify(hh)

## K-means聚类