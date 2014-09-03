library("ROracle")
R.ORCLIP="192.168.5.56"
R.ORCLSID="orcl"
R.ORCLPORT="1521"
R.USERNAME="stat"
R.PASSWORD="stat"
dbstr<-paste("(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=",R.ORCLIP,")(PORT=",R.ORCLPORT,"))(CONNECT_DATA=(SID=\'",R.ORCLSID,"\')))",sep="")
con<-dbConnect(Oracle(),R.USERNAME,R.PASSWORD,dbname=dbstr)
sql <- paste("select * from sh_send_2011 where SEND_NUM >\'",time,"\'")
temp <- dbGetQuery(con,"select * from sh_send_2011")

library("ORE")
r.orclip="192.168.5.56"
r.orclsid="orcl1"
r.orclport="1521"
r.username="rquser"
r.password="apu"
ore.connect(r.username,r.orclsid,r.orclip,r.password,all=T)




