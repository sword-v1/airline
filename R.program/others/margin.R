library(animation)
library(forecast)
setwd("C:\\Users\\Administrator\\Desktop")
temp <- read.csv("margin.csv", header = TRUE)
plot(temp[,2], temp[,3], type = "n")
ani.record(reset = TRUE)
for (i in 1:64) {
  points(temp[i,2], temp[i,3], pch = 18, cex = 1.5)
  ani.record() 
}
oopts = ani.options(interval = 0.5)
ani.replay()

# mg <- ts(temp[,3],1,64)
mg <- ts(temp[,2],1,64)
fit <- auto.arima(mg)
forecast(fit, level=c(80, 95, 99), h=20)
plot(forecast(fit), shadecols="oldstyle")