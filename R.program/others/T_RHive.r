#!/usr/local/bin/Rscript
library(rJava)
library(Rserve)
library(RHive)
#rhive 初始化。
rhive.init();
#rhive连接到hive服务器。
rhive.connect("192.168.3.146");
#查看表信息
rhive.list.tables()
#
rhive.close()

