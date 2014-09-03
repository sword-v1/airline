# 1.收集关注者的信息，整理location信息
library(Rweibo)
roauth <- createOAuth(app_name ="pudding", access_name = "rweibo")
my_fri <- friendships.friends(roauth, uid = 2530951134, count = 200, cursor = 0)
save(my_fri,file = "my_fri.rda")
fri = my_fri[[1]]
info1 = lapply(fri, function(x) c(x$name, x$location, x$followers_count))
info = do.call(rbind, info1)
loc = strsplit(info[, 2], " ")
a = do.call(rbind, loc)

a[, 1][a[, 1] == "台湾"] = "台"
a[, 2][a[, 2] == "台湾"] = "台"
a[, 2][a[, 2] == "其他"] = a[,1][a[,2] == "其他"]

myfri = data.frame(name = info[, 1],province=a[, 1],city=a[, 2],
                   loc = apply(a, 1, paste, collapse = " ", 
                               follower = as.numeric(info[, 3]))
                   myfri = myfri[which(myfri$province != "其他" & myfri$province != "海外"), ]
                   
# 2. 获取并整理经纬度信息                   
library(XML)
# get data from web
webpage <-'http://blog.csdn.net/svrsimon/article/details/8255051'
tables <- readHTMLTable(webpage, stringsAsFactors = FALSE)
raw <- tables[[1]]
zh_posi <- raw[-1, ]
colnames(zh_posi) = c("province", "city", "county", "lon", "lat")
save(zh_posi,file = "zh_posi.rda")
zh_posi$loc = apply(zh_posi[, 1:3], 1, paste, collapse = " ")
zh_posi[, 4:5] = apply(zh_posi[,4:5], 2, as.numeric)
get.loc <- function(loc) {
pro = grepl(loc[1], zh_posi$loc)
cit = grepl(loc[2], zh_posi$loc)
match = which(pro&cit)
show(match)
return(c(mean(zh_posi$lon[match]), mean(zh_posi$lat[match])))  
}
                   
b=apply(myfri[, 2:3], 1, get.loc)
myfri$lon = b[1, ]
myfri$lat = b[2, ]
                  
# 3. 结合Himsic与ggmap包绘制图形                   
library(ggmap)
library(Hmisc)
edgeMaker <- function(whichRow, len = 1, curved = TRUE) {
 fromC <- c(113.27, 23.13) # Origin
 toC <- c(myfri2[whichRow, 3], myfri2[whichRow, 4]) # Terminus
 weight <- myfri2[whichRow, 5] # Terminus
 # Add curve:
 graphCenter <- c(mean(myfri2$m_lon), mean(myfri2$m_lat)) #colMeans(myfri[,1:2])
 #Center of the overall graph
 bezierMid <- c(fromC[1], toC[2]) # A midpoint, for bended edges
 distance1 <- sum((graphCenter - bezierMid)^2)
 if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
                       bezierMid <- c(toC[1], fromC[2])
 } # To select the best Bezier midpoint
 bezierMid <- (fromC + toC + bezierMid)/3   # Moderate the Bezier midpoint
 if(curved == FALSE) 
   bezierMid <- (fromC + toC)/2   # Remove the curve
 edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),   # Generate
 c(fromC[2], bezierMid[2], toC[2]),   # X & y
 evaluation = len)) # Bezier path coordinates
 edge$Sequence <- 1:len   # For size and colour weighting in plot
 edge$weight <- weight
 edge$Group <- whichRow
 return(edge)
 }
 allEdges <- lapply(1:nrow(myfri2), edgeMaker, len = 100, curved = TRUE)
 allEdges <- do.call(rbind, allEdges)[/code]
                   
                   
                   
                   
china=get_map(location = c(lon = mean(myfri2$m_lon), lat = mean(myfri2$m_lat)), 
                          zoom=5,maptype= "roadmap")
 p1=ggmap(china,extent='device',darken=0.2)
 drawit<-function(i) {
  p = p1 + geom_path(data=allEdges[1:i,], 
  aes(x = x, y = y,group = Group, # Edges with gradient
  size=log(weight+1),color=Sequence),
  alpha=0.6,show_guide=F) + # and taper
  scale_colour_gradient(low = "red3", high = "white", guide = "none")
  if (i>=100)
   p = p + geom_point(data = myfri2[1:floor(i/100), ], 
   aes(x = m_lon, y = m_lat, size = log(m_fol + 1) * 1.3),
   alpha = 0.5, show_guide = F, colour = "black") +
   geom_point(data = myfri2[1:floor(i/100), ],
   aes(x = m_lon, y = m_lat, size = (log(m_fol + 1))),
   alpha = 0.6, show_guide = F,colour = "red3")
  return(p)
}
 print(drawit(3800))
                   
                   
# 4. 结合animation包绘制动态图形
library(animation)
saveMovie({
ani.options(interval=.1,
convert = shQuote('C:/Program Files/ImageMagick-6.8.5-Q16/convert.exe'))
for( i in seq(50,3000,50)) 
  print(drawit(i))
})
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   