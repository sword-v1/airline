ip = '192.168.3.129'
sid = 'gtf'
port = '1521'
user = 'adp_dev'
pwd = 'adp_dev'
sqlstr <- "select count(1)  from rpt_report_info a, rpt_report_info_detail b
 where a.id = b.report_info_id
and to_char(b.create_datetime, 'yyyy-MM-dd') > '1970-01-01'
order by b.create_datetime desc, a.create_datetime, b.report_file_name"
t <- system.time(mem_gps_common_readdata(ip=ip,port=port,sid=sid,user=user,pwd=pwd,sqlstr=sqlstr))
cat(t[3],"secs","\n")