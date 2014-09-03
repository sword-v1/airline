require(RCurl)
require(XML)
rm(list =ls())

RJobs <- function(x) {
  jobs <- matrix(0, length(x), 5)
  url1<-c("http://www.indeed.com/jobs?q=%22AnotherTool+R%22&l=united+states&radius=0")
  url2<-c("http://www.indeed.com/jobs?q=%22AnotherTool+or+R%22&l=united+states&radius=0")
  url3<-c("http://www.indeed.com/jobs?q=%22R+AnotherTool%22&l=united+states&radius=0")
  url4<-c("http://www.indeed.com/jobs?q=%22R+or+AnotherTool%22&l=united+states&radius=0")
  url5<- c("http://www.indeed.com/jobs?q=%22AnotherTool+R%22+or+%22AnotherTool+or+R%22+or+%22R+AnotherTool%22+or+%22R+or+AnotherTool+%22&l=United+States&radius=0")
  url <- c(url1, url2, url3, url4, url5)
  url.new <- t(sapply(x, function(x) gsub("AnotherTool", x, url)))
  count.func <- function(page) {
    webpage <- getURL(page)
    webpage <- readLines(tc <- textConnection(webpage)); close(tc)
    aa <- grep("Jobs 1 to ", webpage)
    count <- ifelse (length(aa) == 0, 0, as.integer(gsub("[^0-9]", "", strsplit(webpage[aa], " ")[[1]][7])))
    return(count)
  }
  for (i in 1:length(x)) {
    for (j in 1:5) jobs[i,j] <- count.func(url.new[i,j])
  }
  colnames(jobs) <- c("* R", "* or R", "R *", "R or *", "All")
  rownames(jobs) <- x
  return(jobs)
}

soft <- c("SAS", "SPSS", "Minitab", "Stata", "JMP", "Statistica", "Systat", "BDMP",
          "Python", "Matlab", "Excel", "SQL", "java", "javascript", "perl", "PHP",
          "Fortran", "S-Plus", "Linux", "C%2B%2B", "Access", "Ruby", "Shell","Coffeescript",
          "Gauss") ## C%2B%2B is C++, should replace "AnotherTool" with C%2B%2B to search correctly, not C++
system.time(jobs <- RJobs(soft))
jobs <- jobs[order(-jobs[, 'All']), ]
jobs
