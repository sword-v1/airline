############# Obtain data from UCI
library(stringr)
names <- read.table(file("http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names",encoding='latin1'),comment.char="",sep="\t")
names <- as.vector(as.matrix(names))
names <- names[31:87]
strlist <- str_split(string=names,pattern="_|:")
str3 <- function(x){
  if(x[2]=="freq")
    y <- x[3]
  if(x[2]=="run")
    y <- x[4]
  return(y)
}
names <- lapply(strlist,str3)
names <- unlist(names)
names <- c(names,"class")

# data <- scan("spam.data")
# data <- matrix(data,ncol=58,byrow=T)
data <- read.table(file="http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",sep=",")
names(data) <- names
data$class <- as.factor(data$class)

####### GAM method for classification
library(gam)
y <- data[,58]
x <- data[,1:57]
d <- data.frame(x,y)
out <- glm(y ~ .,family=binomial,data=d)
p <- out$fitted.values
pred <- rep(0,4601)
pred[p > .5] <- 1
e <- table(y,pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)

####### decision tree
library(rpart)
library(rpart.plot)
## Éú³ÉÊ÷
fit <- rpart(class ~ . , data)
par(mfrow=c(1,2), xpd = TRUE)
rpart.plot(fit)
p <- predict(fit,newdata=data)
pred <- rep(0,4601)
pred[p[,2] > .5] <- 1
e <- table(data$class,pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)


## ¼ôÖ¦
zp <- prune(fit, cp = 0.05)
rpart.plot(zp)
p <- predict(zp,newdata=data)
pred <- rep(0,4601)
pred[p[,2] > .5] <- 1
e <- table(data$class,pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)

####### PRIM
# library(prim)
# fit <- prim.box(x=data[,-58],y=data$class)
# summary(fit)

####### mars
library(mda)
fit <- mars(x=data[,-58],y=data$class)
p <- fit$fitted.values
pred <- rep(0,4601)
pred[p > .5] <- 1
e <- table(data$class,pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)