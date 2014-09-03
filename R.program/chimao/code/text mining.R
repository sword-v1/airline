# load R package for text mining
library(tm)
library(Rwordseg)
# set file path
path="C:/Users/Administrator/Desktop/R.program/chimao/data/"

# import sogou dictionary about finance.
installDict(dictpath=paste0(path,"财经金融词汇大全【官方推荐】.scel"),dictname = "finance",dicttype="scel")

# read data from .csv files
mydata0 <- read.csv(paste0(path,"提取关键词 第二列.csv"),header=FALSE)
mydata <- as.character(mydata0[,2])

# word segmentation
txt0 <- segmentCN(mydata) 
txt <- Corpus(VectorSource(txt0))
dtm <- DocumentTermMatrix(txt,control=list(dictionary=NULL,removePunctuation = TRUE,stopwords=TRUE, wordLengths = c(1, Inf)))
# dtm <- removeSparseTerms(dtm, sparse=0.9)
data0 <- as.data.frame(inspect(dtm))

# delete meaningless words
movewords <- c("基于","和","及","的","与","以","在","为")
data <- data0[,!colnames(data0) %in% movewords]

# extract keywords
keywords <- lapply(txt0,FUN=function(x,move=movewords){
  paste(x[!x %in% movewords],collapse=",")
} )
keywords <- unlist(keywords)
mydata0 <- cbind(mydata0,keywords)

# order by word frequency
freqM <- data.frame(Name=colnames(data),Freq=drop(apply(data,2,sum)),row.names=1:ncol(data))
freqM <- freqM[order(freqM$Freq,decreasing=T),]

# output 
write.csv(data0,paste(path,"DocumentTerm.csv")) 
write.csv(freqM,paste(path,"freqM.csv"),row.names=FALSE)
write.table(mydata0,file=paste(path,"keywords.csv"),sep=",",row.names=FALSE,col.names=FALSE)