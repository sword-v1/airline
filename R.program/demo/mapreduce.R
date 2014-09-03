# mapreduce function
wordcount = function(input,
                     output = NULL,
                     pattern = " "){
  wc.map = function(., lines) {
    keyval(unlist(strsplit(x = lines,split = pattern)),1)}
  wc.reduce = function(word, counts ) {keyval(word, sum(counts))}
  mapreduce(input = input ,
            output = output,
            input.format = "text",
            map = wc.map,
            reduce = wc.reduce,
            combine = T)}

# execute the RHadoop MapReduce job
wordcount('/RHadoop/1/')

# Exploring the wordcount output:
from.dfs("/tmp/RtmpRMIXzb/file2bda5e10e25f")