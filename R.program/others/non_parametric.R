################################
kernel<-function(x){
  return ((15/16)*(1-x^2)^2*(abs(x)<=1))
}
##################################
weight<-function(t.val,t,h){
  return (kernel((t.val-t)/h)/sum(kernel((t.val-t)/h)))
}
#########################
n <- 1000;c <- 1
t <- runif(n);eps <- rnorm(n,0,0.1)
y <- sin(2*pi*t)+eps
tsd<-sqrt(var(t))
h<-c*tsd*n^(-1/5)
y.po <- rep(0,n);x.po <- rep(0,n)
for(i in 1:n){
  y.po[i]<-sum(weight(t[i],t,h)*y)
}	
x <- cbind(t,y)
x1 <- cbind(t,y.po)
plot(x[order(x[,1]),])
points(x1[order(x1[,1]),], type = "l", col = 2)
