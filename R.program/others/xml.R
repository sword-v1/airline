library(XML)
# xmlfile<-xmlTreeParse("C:\\Users\\Administrator\\Desktop\\sh600125_20.xml", encoding = "latin1")
xmlfile<-xmlTreeParse("C:\\Users\\Administrator\\Desktop\\sh600125_20.xml", encoding = "utf-8")
r <- xmlRoot(xmlfile)
kdata <- iconv(xmlValue(r[[1]]), from = 'UTF-8', to = '')
data  <- unlist(strsplit(kdata, "\t|\n"))
data  <- data[-1]
names <- head(data,5)
data <- data[-c(1:5)]
dim(data) <- c(5,length(data)/5)
data <- t(data)
data <- as.data.frame(data)
names(data) <- names
write.csv(data,"C:\\Users\\Administrator\\Desktop\\sh600125_20.csv", row.names = FALSE)
