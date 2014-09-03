## read raw data
rawdata <- read.csv("C:\\Users\\Administrator\\Desktop\\sz300274_10.csv", header = TRUE)

## bolling line
## test_BollingLine <- BollingLine(theta = c(1.96,2,3,4)) -- running this get result
BollingLine <- function(data = rawdata, theta , nday = 20){
  closep <- rawdata$close
  ntheta <- length(theta)
  m <- nrow(rawdata)
  for (j in 1:ntheta){
    n <- theta[j]
    boll <- rep(NA, m)
    upper <- rep(NA, m)
    lower <- rep(NA, m)    
    for(i in nday:m){
      temp <- closep[(i-nday+1):i]
      boll[i] <- mean(temp)
      upper[i] <- mean(temp) + n*sd(temp)
      lower[i] <- mean(temp) - n*sd(temp)
    }
    rawdata[paste("boll",theta[j],sep = '')] <- boll
    rawdata[paste("upper",theta[j],sep = '')] <- upper
    rawdata[paste("lower",theta[j],sep = '')] <- lower
    
  }
  return(rawdata)
}

## Williams %R 
## tt <- cbind(WR(nday = 3,nmov = c(2,5)), wr_10 = WR(nday = 10,nmov = 1)$wr)[,-6]
WR <- function(data = rawdata, nday , nmov){
  m <- nrow(rawdata)
  wr <- rep(NA, m)
  for(i in nday:m){
    temp <- rawdata[c((i-nday+1):i),]
    if(max(temp$high) - min(temp$low) == 0){
      wr[i] <- wr[i-1]
    }else{
      wr[i] <- 100*(max(temp$high) - temp$close[nday])/(max(temp$high) - min(temp$low))     
    }
  }
  rawdata$wr <- wr
  l <- length(nmov)
  for (k in 1:l){
    dt <- nmov[k]
    mv <- rep(NA, m)
    for (j in (nday+dt-1):m){
      temp <- wr[c((j-dt+1):j)]
      mv[j] <- mean(temp)
    }
    colname <- paste("wr",nday,sep = "_")
    colname <- paste(colname, "mv", sep = ".")
    mv -> rawdata[paste(colname, dt, sep = '_')]
  }
  return(rawdata)
}
