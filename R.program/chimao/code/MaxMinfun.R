path <- "C:/Users/Administrator/Desktop/"
dat <- read.csv(paste0(path,"600149.csv"),header=T)
names(dat) <- c("date","price")
dat$date <- as.POSIXct(dat$date)
plot(dat,cex=.5)
# lines(smooth.spline(dat$date,dat$price),col="red")
# fit <- smooth.spline(dat$date,dat$price)

## Maximums and minimums for local data in a time range.
MaxMinfun <- function(x,start,end){
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  if(min(x$date) <= start & start < end & end <= max(x$date)){
    ind <- which(x$date >= start & x$date < end)
    pmax <- max(x$price[ind])
    pmin <- min(x$price[ind])
    res <- c(pmin,pmax)    
  } else {
    stop("There is something wrong with input date.")
  }
  return(res)
}

## For example
MaxMinfun(x=dat,start='2014-03-01',end='2014-04-01')

## extremum for whole data
## the bandwidth can be adjusted.
extrefun <- function(x,bandwidth=5){
  n <- length(x)
  extremum <- c()
  ind <- c()
  for(i in 1:n){
    if(i<=bandwidth){
      tmp <- x[1:(i+bandwidth)]
    } else if(i>bandwidth & i<n-bandwidth){
      tmp <- x[(i-bandwidth):(i+bandwidth)]
    } else {
      tmp <- x[(i-bandwidth):n]
    }
    if(x[i] == max(tmp)|x[i] == min(tmp)){
      extremum <- c(extremum,x[i])
      ind <- c(ind,i)
    }
  }
  res <- list()
  res$extremum <- extremum
  res$ind <- ind
  return(res)
}

res <- extrefun(x=dat$price,bandwidth=10)
plot(dat,cex=.5)
points(dat$date[res$ind],res$extremum,col="red",cex=.5)
temp <- data.frame(index=res$ind,date=dat$date[res$ind],extremum=res$extremum)
write.csv(temp,paste0(path,"Etrm","600149.csv"),row.names=FALSE)
