library(tm)
library(Rwordseg)
library(wordcloud)
sysmss <- read.table("C:\\Users\\Administrator\\Desktop\\star.txt", header = FALSE)
n = length(sysmss[, 1])
# sysmss = strsplit(sysmss, "£¬|¡£|£º|¡¢|!")
sysmss = sysmss[sysmss!=" "]
sysmss = iconv(sysmss,"utf-8","gbk")
insertWords(c("ÆßÓÖÆß·ÖÖ®Ò»","ÓÂ¸ç","º¬Ã«Æß²½µø","¿­Ã«","´óÁÁ","ÁÁÁÁ","Ð¡·è","»ªÒ¯",
              "ÕÅÔË","¶¹¶¹","³ÕÃ«","ºÆ¸ç","Ð¡s","¶Ó³¤±ð¿ªÇ¹","°¢·É","Ð¡Âí¸ç"))
#sysmss = sysmss[nchar(sysmss)!=10&nchar(sysmss)!= 9]
sysmss = sysmss[nchar(sysmss)>10]
sysmss = unlist(segmentCN(sysmss))
sysmss <- sysmss[nchar(sysmss)>=2&nchar(sysmss)<=6]
# sysmss = unlist(strsplit(as.character(sysmss), ", "))
v = table(unlist(sysmss))
v = sort(v, deceasing = T)
d = data.frame(word = names(v), freq = v)
# i <- which(nchar(t)==9)
d <- d[d$freq>=10,]
d <- tail(d,100)
wordcloud(tt$word, tt$freq, scale = c(4,0.33), min.freq = 10,max.words = 200,
          random.order = FALSE, random.color = TRUE, colors = c(1:5))

library("Rweibo", lib.loc="d:/R/R-2.15.2/library")
registerApp(app_name = "Rweib", "1168196103", "6d486cabfacd7ae86e90bf9b7481f356")
listApp("Rweib")
roauth <- createOAuth(app_name = "Rweib", access_name = "sword_v1")
res11 <- web.search.content("RÓïÑÔ", page = 2, combinewith = NULL)
