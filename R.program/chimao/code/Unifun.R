code <- 600149 # you can change code by this word.
path <- "C:/Users/Administrator/Desktop/"
dat <- read.csv(paste0(path,code,".csv"),header=T)
names(dat) <- c("date","price")
dat$date <- as.POSIXct(dat$date)
dat <- dat[order(dat$date),]

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
  nod <- c()
  for(i in 1:n){
    if(i<=bandwidth){
      tmp <- x[1:(i+bandwidth)]
    } else if(i>bandwidth & i<n-bandwidth){
      tmp <- x[(i-bandwidth):(i+bandwidth)]
    } else {
      tmp <- x[(i-bandwidth):n]
    }
    if(x[i] == max(tmp)){
      extremum <- c(extremum,x[i])
      ind <- c(ind,i)
      nod <- c(nod,"h")
    }
    if(x[i] == min(tmp)){
      extremum <- c(extremum,x[i])
      ind <- c(ind,i)
      nod <- c(nod,"l")
    }
  }
  res <- list()
  res$extremum <- extremum
  res$ind <- ind
  res$nod <- nod
  return(res)
}

# paste a string identity for highs and lows.
strfun <- function(nod,ind,n.max){
## n.max is the length of raw data.
  if(ind[length(ind)]==n.max){
    freq <- c(ind[1],diff(ind))
    n <- length(freq)
    res <- c()
    for(i in 1:n){
      res <- c(res,rep(nod[i],freq[i]))
    }    
  }
  if(ind[length(ind)]<n.max){
    freq <- c(ind[1],diff(c(ind,n.max)))
    if(nod[length(nod)]=="h"){
      nod <- c(nod,"l")
    } else {
      nod <- c(nod,"h")
    }
    n <- length(freq)
    res <- c()
    for(i in 1:n){
      res <- c(res,rep(nod[i],freq[i]))
    }      
  }
  return(res)
}

## 1st.compute the extremum
res <- extrefun(x=dat$price,bandwidth=10)
res

## 2nd.output into excel
temp <- data.frame(index=res$ind,date=dat$date[res$ind],extremum=res$extremum,nod=res$nod)
write.csv(temp,paste0(path,"Etrm",code,".csv"),row.names=FALSE)

dat$nod <- strfun(nod=res$nod,ind=res$ind,n.max=nrow(dat))
write.csv(dat,paste0(path,"Corr",code,".csv"),row.names=FALSE)


## 3rd.plot
png(paste0(path,code,".png"),width=1200,height=800)
plot(dat$date,dat$price,cex=.5,main=code,xlab="Date",ylab="price")
points(dat$date[res$ind],res$extremum,col="red",cex=1,pch=8)
ind_l <- res$nod=="l"
ind_h <- res$nod=="h"
text(dat$date[res$ind[ind_l]],res$extremum[ind_l],labels=res$extremum[ind_l],pos=1,cex=.8)
text(dat$date[res$ind[ind_h]],res$extremum[ind_h],labels=res$extremum[ind_h],pos=3,cex=.8)
dev.off()

