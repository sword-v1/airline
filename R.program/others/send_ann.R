library(RSNNS)
library("ROracle")
R.ORCLIP="192.168.5.56"
R.ORCLSID="orcl"
R.ORCLPORT="1521"
R.USERNAME="stat"
R.PASSWORD="stat"
dbstr<-paste("(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=",R.ORCLIP,")(PORT=",R.ORCLPORT,"))
             (CONNECT_DATA=(SID=\'",R.ORCLSID,"\')))",sep="")
con<-dbConnect(Oracle(),R.USERNAME,R.PASSWORD,dbname=dbstr)
sh.send <- dbGetQuery(con,"select * from sh_bureau_unit_2011")
sh.send <- sh.send[,c(1,2)]

# 建立节假日标识
hld.def1 <- data.frame(newyear = c('20110101','20110103'),
                tombsweeping = c('20110401','20110405'),
                labor = c('20110428','20110502'),
                dargon = c('20110603','20110606'),
                midautumn = c('20110908','20110912'),
                national = c('20110928','20111007'),
                newyear = c('20111231','20111231'),
                stringsAsFactors = FALSE)

# 建立春节标识
hld.def2 <- data.frame(spring1 = c('20110112','20110201'),
                 spring = c('20110202','20110208'),
                 spring2 = c('20110209','20110227'),
                 stringsAsFactors = FALSE)

# 计算春运暑运以及节假日特征
day.hld <- rep(0, times = nrow(sh.send))
day.spr <- rep(0, times = nrow(sh.send))
for (i in 1:nrow(sh.send)){
  day.hld[(sh.send$TRAIN_DATE >= hld.def1[1,i]) & (sh.send$TRAIN_DATE <= hld.def1[2,i])] <- 1
  day.spr[(sh.send$TRAIN_DATE >= hld.def2[1,i]) & (sh.send$TRAIN_DATE <= hld.def2[2,i])] <- 1
}

train.date <- strptime(sh.send$TRAIN_DATE,"%Y%m%d")
sh.send$DATE <- train.date
wday <- train.date$wday 
wday[wday == 0] <- 7
sh.send$wday <- wday 
prime.send <- as.data.frame(head(cbind(wday,send = sh.send$SEND_NUM),7))

# 日趋势特征以及周期特征
day.prm <- prime.send$send 
day.trd <- day.prm[1] + 0.0477*train.date$yday
day.cle <- rep(0,nrow(sh.send))
for (i in 1:nrow(sh.send)){
  day.cle[i] <- prime.send$send[prime.send$wday == sh.send$wday[i]] + 0.5479*(train.date$yday[i] - 7)
}

pre.send <- as.data.frame(cbind(day.trd,day.cle,day.spr,day.hld,send = sh.send$SEND_NUM))
#将数据顺序打乱
pre.send <- pre.send[sample(1:nrow(pre.send),length(1:nrow(pre.send))),1:ncol(pre.send)]
#定义网络输入
pre.send.values <- pre.send[,1:4]
#定义网络输出，并将数据进行格式转换
max.send <- max(pre.send[,5])
pre.send.targets <- pre.send[,5]/max.send
#从中划分出训练样本和检验样本
pre.send <- splitForTrainingAndTest(pre.send.values, pre.send.targets, ratio = 0.15)
#数据标准化
pre.send <- normTrainingAndTestSet(pre.send)
#利用mlp命令执行前馈反向传播神经网络算法
model <- mlp(pre.send$inputsTrain, pre.send$targetsTrain, size=5, learnFunc="Quickprop", 
             learnFuncParams=c(0.1, 2.0, 0.0001, 0.1), maxit=100, inputsTest = pre.send$inputsTest,
             targetsTest = pre.send$targetsTest)
#利用上面建立的模型进行预测
predictions <- predict(model, pre.send$inputsTest)*max.send
#predict.error <- as.data.frame(cbind(tail(sh.send$SEND_NUM,55),predictions))
#predict.error <- (predict.error[,2] - predict[,1])/predict.error[,1]




