## simulation for additive model
library(gam)
library(mgcv)
set.seed(123)
n <- 1000
x1 <- rnorm(n,0,1)
x2 <- runif(n,-2,2)
x3 <- rnorm(n,0,1)
x4 <- runif(n,-2,2)
f1 <- sin(x1)
f2 <- x2^2
f3 <- x3^2 + 1
f4 <- x4^3 + 2*x4^2 +1
y <- f1 + f2 + f3 +f4 + rnorm(n, 0 ,1) 
data1 <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, y = y)
mod.sim <- gam(y ~ s(x1) + s(x2) + s(x3) + s(x4), data = data1)
plot(mod.sim, pages = 1, se = TRUE, residuals=TRUE, ylim = c(-3,3))
# predict.gam(mod.sim, newdata=data1)
res <- residuals(mod.sim)
ks.test(res, "pnorm", 0, 0.1)

## function for calibrate 
Std <- function(TAT,ALT,N1,N2,EGT,FF,PT2)
{
  theta <- (TAT+273.15)/288.15
  delta <- PT2/14.696
  ALTC <- ALT*0.3048
  EGTC <- (EGT + 273)/theta^0.979 -273
  FFC <- FF/(delta*theta^0.692)
  N1C <- N1/theta^0.489
  N2C <- N2/theta^0.489
  
  res=data.frame(TATC = theta, ALTC, PT2C = delta, EGTC,FFC,N1C,N2C)
  return(res)
}

## read ENG B-5171 data
Boeing.data  <- read.csv("C:\\Users\\Administrator\\Desktop\\SMART\\·¢¶¯»ú\\import_table\\5171.csv")
ENG1 <- with(Boeing.data,{res <- Std(TAT, ALT, N1.1, N2.1, EGT.1, FF.1, PT2.1)
          ENG1 <- data.frame(EHRS = EHRS.1, res, MN)
}) 
# ENG2 <- with(Boeing.data,{res <- Std(TAT, ALT, N1.2, N2.2, EGT.2, FF.2, PT2.2)
#                           ENG1 <- data.frame(EHRS = EHRS.2, res, MN)
# })

## Data Partition
mod.sim <- gam(EGTC ~ s(EHRS) + s(N1C) + s(ALTC) + s(TATC) + s(MN), data = ENG1)
plot(mod.sim, pages = 1, se = TRUE, resid=TRUE)


## Cross Validation
library(cvTools)
cv <- cvFit(mod.sim, data = ENG1, y = ENG1$EGTC, cost = rtmspe,
      K = 3, R = 1, costArgs = list(trim = 0), seed = 1234)






