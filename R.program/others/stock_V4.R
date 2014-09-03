stock <- function(){
  setwd("C:\\Users\\Administrator\\Desktop\\CSV文件批量导入R")
  id <- dir()
  theta_value <- c(5,8,10,12,15,20,30,40,60)
  sub_id <- unique(substr(id,1,8))
  for (k in 1:length(theta_value)){
    theta <- theta_value[k]
    for (l in 1:length(sub_id)){
      sub_set <- id[substr(id,1,8) == sub_id[l]]
      final <- matrix(0,0,6)
      colnames(final) <- c("date","time","open","high","low","close")
       for (j in 1:length(sub_set)){
         data <- read.csv(sub_set[j],header = TRUE)
         data <- as.data.frame(data[c(1,2)])
         names(data) <- c("time","price")
         time <- as.character(as.factor(data$time))
         # time <- as.POSIXct(time,"%T")
         data$time <- strptime(time,"%T")
         ## AM 
         first.time <- strptime(as.character("9:30:00"),"%T")
         n <- 120/theta
         final.data <- matrix(0,2*n,6)
         colnames(final.data) <- c("date","time","open","high","low","close")
         final.data[,1] <- substr(sub_set[j],15,24)
         i <- 1
         while(i <= n ){
           if(i == 1){
             temp <- subset(data,time <= first.time + i*theta*60,price)
             temp <- rev(as.vector(as.matrix(temp)))
             final.data[i,2] <- format(first.time + i*theta*60,"%H:%M")
             final.data[i,3] <- temp[1]
             final.data[i,4] <- max(temp)
             final.data[i,5] <- min(temp)
             final.data[i,6] <- temp[length(temp)]  
           }else{
             temp1 <- subset(data,time > first.time + (i-1)*theta*60 & time <= first.time + i*theta*60,price)  
             temp1 <- rev(as.vector(as.matrix(temp1)))
             final.data[i,2] <- format(first.time + i*theta*60,"%H:%M")
             final.data[i,3] <- temp1[1]
             final.data[i,4] <- max(temp1)
             final.data[i,5] <- min(temp1)
             final.data[i,6] <- temp1[length(temp1)]
           }
           i <- i+1
         }
         ## PM
         second.time <- strptime(as.character("13:00:00"),"%T")
         i <- 1
         while(i <= n ){
           if(i == 1){
             temp <- subset(data,time <= second.time + i*theta*60,price)
             temp <- rev(as.vector(as.matrix(temp)))
             final.data[n+i,2] <- format(second.time + i*theta*60,"%H:%M")
             final.data[n+i,3] <- temp[1]
             final.data[n+i,4] <- max(temp)
             final.data[n+i,5] <- min(temp)
             final.data[n+i,6] <- temp[length(temp)]  
           }else if(i < n){
             temp1 <- subset(data,time > second.time + (i-1)*theta*60 & time <= second.time + i*theta*60,price)  
             temp1 <- rev(as.vector(as.matrix(temp1)))
             final.data[n+i,2] <- format(second.time + i*theta*60,"%H:%M")
             final.data[n+i,3] <- temp1[1]
             final.data[n+i,4] <- max(temp1)
             final.data[n+i,5] <- min(temp1)
             final.data[n+i,6] <- temp1[length(temp1)]
           }else{
             temp2 <- subset(data,time > second.time + (i-1)*theta*60,price)  
             temp2 <- rev(as.vector(as.matrix(temp2)))
             final.data[n+i,2] <- format(second.time + i*theta*60,"%H:%M")
             final.data[n+i,3] <- temp2[1]
             final.data[n+i,4] <- max(temp2)
             final.data[n+i,5] <- min(temp2)
             final.data[n+i,6] <- temp2[length(temp2)]
           }
           i <- i+1
         }
         final <- rbind(final,final.data)
       }
      b <- substr(id[j],25,28)
      file <- paste(sub_id[l], theta, sep = "_")
      file <- paste(file, b, sep = "")
      write.csv(final,file,row.names = FALSE)       
    }
  } 
}
