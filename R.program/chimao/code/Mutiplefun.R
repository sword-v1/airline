code <- "多列数据" ##change file name according to this.
path <- "C:/Users/Administrator/Desktop/"
dat <- read.csv(paste0(path,code,".csv"),header=T)
dat["日期"] <- as.POSIXct(as.character(unlist(dat["日期"])))
dat <- dat[order(dat["日期"]),]

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

np <- dim(dat)
Mhl <- dat
for(i in 2:np[2]){
  temp <- dat[,i]
  res <- extrefun(x=temp,bandwidth=10)
  Mhl[,i] <- strfun(nod=res$nod,ind=res$ind,n.max=np[1])
}

write.csv(Mhl,paste0(path,code,"涨跌矩阵",".csv"),row.names=FALSE)




