library(TSA)
data <- read.csv("C:\\Users\\Administrator\\Desktop\\上海虹桥客流量预测a.csv",header = TRUE)
train <- head(data[,2],335)
test <- tail(data[,2],30)
train <- ts(train, start = 1, frequency = length(train)) 
#fore=predict(arimax(x=dat,order=c(0,1,8),seasonal=list(order=c(0,1,8),fix=c(NA,rep(0,5),
#             NA,NA,0),period=7)),(length(dat)+60),xreg=1:(length(dat)+60))

fite <- arima(x=train,order=c(0,1,8),seasonal=list(order=c(0,1,8),fix=c(NA,rep(0,5),NA,NA,0),
           period=7))
fitted <- fitted(fite)
pred <- predict(fite,30)
pred <- cbind(test,pred$pred)

library("Rweibo", lib.loc="d:/R/R-2.15.3/library")
roauth <- createOAuth(app_name = "Rweib", access_name = "sword_v1")
res1 <- web.search.content("R语言", page = 2, combinewith = NULL)
res2 <- statuses.user_timeline(roauth, screen_name = "LV-Eric", count = 5)
# 
# library(reshape2)
# id <- 1:nrow(iris) 
# iris.lng <- melt(iris, id=c("id", "Species"))
# head(iris.lng)    
# iris.wide <- dcast(iris.lng, id + Species ~ variable)head(iris.wide)
# head(iris.wide)
# library(ggplot2)
# p <- ggplot(aes(x=value, fill=Species), data=iris.lng)p + geom_histogram() +  facet_wrap(~variable, scales="free")

library(ff)
v = ff(vmode = "double", length = 1e+08)