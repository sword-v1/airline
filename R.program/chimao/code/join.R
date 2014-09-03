# Example
dat1 <- data.frame(name=c("Pual","Ray","James","Kobe"),height=c(183,196,206,198))
dat2 <- data.frame(date=as.Date((Sys.Date()-7):Sys.Date(),origin="1970-01-01"),name=c("Pual","Ray","James","Kobe","James","James","Kobe","Pual"),score=c(22,18,32,26,36,48,31,18))

## 1st.Using merge function.
dat <- merge(dat1,dat2)


## 2st.Loading sqldf package, query by writing sql sentence in R.
require(sqldf)
sqlstr <- "select * from dat1 d1,dat2 d2 where d1.name=d2.name"
dat <- sqldf(sqlstr)


