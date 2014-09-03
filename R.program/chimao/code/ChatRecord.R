##### First. 数据预处理 #####
# 将向量的字符进行粘贴合并成一个字符
Mg <- function(X){ 
  # X is a Vector
  n  <- length(X)
  res <- c()
  for (i in 1:n){
    res  <- paste(res,X[i],seq="")
  }
  return(res)
}

# 读入.txt文本数据
talking_data <- read.table("C:\\Users\\user\\Desktop\\1大学群(77298285).txt", header = FALSE , blank.lines.skip = FALSE)
# 利用正则表达式匹配日期数据
talking_time <- grep("20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]",talking_data[,1])
# 整理聊天内容
target_data1 <- cbind(talking_data[talking_time,],content = NA)
target_data1$id <-row.names(target_data1)
id <- c(as.numeric(target_data1$id),nrow(talking_data))
n <- length(id)-1
for (i in 1:n){
  temp <- as.vector(t(as.matrix(talking_data[(id[i]+1):(id[i+1]-1),])))
  temp <- Mg(temp)
  target_data1[i,4] <- temp
}

# 把日期和时间整理为一个字段,调整字段的类型
time <- paste(target_data1$V1,target_data1$V2)
target_data1$time <- time
target_data1 <- target_data1[-1,]
pretreat <- subset(target_data1, select=c(time,V3,content))
# 清楚占内存的对象
rm(list=ls()[ls()!="pretreat"])
# 规约字段的数据类型— time为时间型,其他为字符型
names(pretreat)[2] <- "id"
pretreat$time <- as.POSIXct(pretreat$time)
pretreat$id <- as.character(pretreat$id)
pretreat$content <- as.character(pretreat$content)


#####【分析画图】#####

###气泡图###
crime<-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",header=TRUE,sep="\t")
N <- nrow(crime)
symbols(crime$murder,crime$burglary,circles=crime$population) #绘制气泡图，x为谋杀率，y为入室盗窃率，气泡半径为人口数量
radius<-sqrt(crime$population/pi) # 圆圈大小
symbols(crime$murder,crime$burglary,circles=radius,inches=0.35,
        fg="white",bg="red",xlab="Murder Rate",ylab="Burglary Rate") # fg-边框，bg-填充颜色，inches-设定大小
symbols(crime$murder,crime$burglary, squares=radius,inches=0.8,fg="white",bg = rgb(c(0.7,0.5,0.8,0.4),c(0.1,0.5,0.8,0.4),c(0.2,0.5,0.8,0.4),0.3)) #正方形
text(c(6.9,6.2), c(693.3,931.0), c("CAL","GEO"), cex=0.8, col = c("blue","red"))

###热点图###
bball<-read.csv("http://datasets.flowingdata.com/ppg2008.csv",header=TRUE) #按PTS高低排序后的
#希望每一行的名称应该是球员的姓名，而不是现在的行号，需要把第一列设置为行名
row.names(bball) <- bball$Name
bball<-bball[,2:20]
#数据必须是矩阵格式，不能呢个是数据框dataframe格式，否则heatmap()会报错
bball_matrix<-data.matrix(bball)
bball_heatmap<-heatmap(bball_matrix,Rowv=NA,Colv=NA,col=cm.colors(256),scale="column",margins=c(5,10))
bball_heatmap<-heatmap(bball_matrix,Rowv=NA,Colv=NA,col=heat.colors(256),scale="column",margins=c(5,10))
