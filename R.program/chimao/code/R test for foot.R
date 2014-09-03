# x1 <- c(2.74,2.95,2.38)
# x2 <- c(1.7,3.35,4.25)

x1 <- c(2.72,2.63)
x2 <- c(3.06,2.38)


optpp <- function(x1,x2){
  n1 <- 1:10
  n2 <- 1:10
  fval <- matrix(0,length(n1)*length(n2),1+length(x1))
  for(k in 1:length(n1)){
    for(l in 1:length(n2)){
      tmp <- rep(0,length(x1)*length(x2))
      for(i in 1:length(x1)){
        for(j in 1:length(x2)){
          tmp[(i-1)*length(x1)+j] <- 2*(n1[k]*x1[i] + n2[l]*x2[j])
        }
      }
      cat(tmp,"\n")
      fval[(k-1)*length(n1)+l,1] <- min(tmp)/(2*(n1[k]+n2[l])) 
      fval[(k-1)*length(n1)+l,2] <- k
      fval[(k-1)*length(n1)+l,3] <- l
    }
  }
  return(fval)
}

temp <- optpp(x1,x2)
temp[which.max(temp[,1]),]