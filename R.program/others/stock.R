stocks <- read.csv("stocks.csv",header = TRUE)
fx <- function(x,theta=3,k=3){
  n <- length(x)
  t1 <- rep(0,n)
  t2 <- rep(0,n)
  if (theta > n)
    cat("The length is too big.")
  else {
    for (i in 1:(n-theta+1)){
      t1[i+theta-1] <- mean(x[i:(i+theta-1)])
      t2[i+theta-1] <- sd(x[i:(i+theta-1)])
    }
    t <- cbind(t1,t2)  
    return(tail(t,k))
  }
}
apply(stocks[,-1],2,fx)