##  generate data
set.seed(123) 
n <- 1000  # sample size 
m <- 200   # number of testing data
x1 <- runif(n,1,100)
x2 <- runif(n,1,100)
y <- 3*x1 - 2*x2 + 4 + rnorm(n,0,1)
dat0 <- data.frame(x1=x1,x2=x2,y=y)
dattr <- dat0[1:(n-m),]
dattt <- dat0[(n-m+1):n,]

## build linear regression model
lmfit <- lm(formula=y~x1+x2,data=dattr)
summary(lmfit)

# predict 
pred <- predict(lmfit,newdata=dattt)
res <- dattt$y - pred

# plot
library("scatterplot3d")
z <- c(dattt$y,pred)
x <- c(dattt$x1,dattt$x1)
y <- c(dattt$x2,dattt$x2)
colour <- c(rep("blue",m),rep("red",m))
scatterplot3d(x, y, z,color=colour, col.axis="blue",col.grid="lightblue", main="scatterplot3d", pch=20)