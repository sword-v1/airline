data = read.table('C:\\Users\\Administrator\\Desktop\\data.txt', sep=',', header=TRUE)
# 标识user与subject的索引 
user = unique(data$user_id) 
subject = unique(data$subject_id) 
uidx = match(data$user_id, user) 
iidx = match(data$subject_id, subject) 
# 从二元组构造收藏矩阵 
M = matrix(0, length(user), length(subject)) 
i = cbind(uidx, iidx) 
M[i] = 1 
# 对列向量（subject向量）进行标准化，%*%为矩阵乘法 
mod = colSums(M^2)^0.5
# 各列的模 
MM = M %*% diag(1/mod) 
# M乘以由1/mod组成的对角阵，实质是各列除以该列的模 
#crossprod实现MM的转置乘以MM，这里用于计算列向量的内积，S为subject的相似度矩阵
S = crossprod(MM) 
# user-subject推荐的分值 
R = M %*% S 
R = apply(R, 1, FUN=sort, decreasing=TRUE, index.return=TRUE) 
k = 5
# 取出前5个分值最大的subject 
res = lapply(R, FUN=function(r)return(subject[r$ix[1:k]]))
# 输出数据 
write.table(paste(user, res, sep=':'), file='C:\\Users\\Administrator\\Desktop\\result.dat', quote=FALSE, row.name=FALSE, col.name=FALSE)
