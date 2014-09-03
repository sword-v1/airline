dat <- read.csv("C:\\Users\\Administrator\\Desktop\\sh600970_2Ìì.csv",header = TRUE)
dat$time <- as.POSIXct(dat$time)
intervel <- as.POSIXct(c("2013-03-20","2013-06-14"))
res <- dat[dat$time>=intervel[1]&dat$time<=intervel[2],]