## $Id: bollingerBands.R,v 1.3 2005/07/24 17:04:20 edd Exp $
## cf with tseries:::get.hist.quote() and its::priceIts()
# getData <- function(instrument = "IBM",
#                     start=format(Sys.time()-60*60*24*252,
#                                  "%Y-%m-%d"),      ## 200 days ago
#                     end=format(Sys.time(), "%Y-%m-%d"),   ## today
#                     quote = c("Open","High", "Low", "Close", "Volume"),
#                     method = "auto",
#                     origin = "1899-12-30",
#                     compression="d",
#                     quiet=TRUE) {
#   start <- as.POSIXct(start,tz="GMT")   # store date as POSIXct
#   end <- as.POSIXct(end,tz="GMT")
#   url <-                                # form URL for download
#     paste("http://chart.yahoo.com/table.csv?s=",
#           instrument,
#           format(start,
#                  paste("&a=",as.character(as.numeric(format(start, "%m")) - 1),
#                        "&b=%d&c=%Y", sep = "")),
#           format(end,
#                  paste("&d=", as.character(as.numeric(format(end, "%m")) - 1),
#                        "&e=%d&f=%Y", sep = "")),
#           "&g=", compression,
#           "&q=q&y=0&z=", instrument, "&x=.csv", sep = "")
#   destfile <- tempfile()                # and download to tempfile
#   status <- download.file(url, destfile, method = method, quiet=quiet)
#   if (status != 0) {
#     unlink(destfile)
#     stop(paste("download error, status", status))
#   }
#   nlines <- length(count.fields(destfile, sep = "\n"))
#   if (nlines == 1) {
#     unlink(destfile)
#     stop(paste("No data available for", instrument))
#   }
#   v <- readLines(destfile)
#   return(v)
# }
#   #cl <- grep("^ X$Open")
#   v <- readLines(destfile)
#   X <- data.frame(v)
#   x <- 1:nrow(X)
#   ind <- which(X$Close >= X$Open)
#   segments(x[ind], X[ind,"Open"], x[ind], X[ind,"Close"],col="green", lwd=2)
#   ## part three: for losing days where close is lower than open, plot
#   ## red bars showing retreat from the open to the close
#   ind <- which(X$Close < X$Open)
#   segments(x[ind], X[ind,"Open"], x[ind], X[ind,"Close"],col="red", lwd=2)
#   ## part four: blue bars for the days intraday retreat from the lower
#   ## of Open and Close to the Low
#   segments(x, X$Low, x, apply(X[,c("Open","Close")], 1, min),col="blue", lwd=2)
#   axis(2)
#   #axis(4, pos=par("usr")[1], line=0.5)  # this would plot them 'inside'
#   title(ylab="log(Price)")              # y-axis label
#   box()                                 # outer box
#   par(oldpar)
# }

plotBollingerIndicators <- function(X) {
  bbwidth <- 100*(X$bbupper-X$bblower)/X$bbmiddle
  bbpct <- (X$close-X$bblower)/(X$bbupper-X$bblower)
  x <- 1:NROW(X)
  oldpar <- par(mar=c(0,4,0,4))         # no top spacing
  plot(x, bbwidth, col='blue', ylab="", type='l', axes=FALSE, xaxs="i",
       ylim=c(min(0, min(bbwidth)) ,max(bbwidth)))
  abline(h=100, lty='dotted', col='blue')
  grid()
  title(ylab="Bandwidth", col.lab='blue') # title the y-axis in blue too
  axis(2, col.axis='blue')              # y-axis
  par(new=TRUE)                         # add to the plot
  plot(x, bbpct, col='red', type='l', ylab="", axes=FALSE, xaxs="i")
  abline(h=c(0,1), lty='dotted', col='red')
  axis(4, col.axis='red')
  ## need mtext() to annotate 2nd y-axis as title() doesn't do it
  mtext("%b", side=4, col='red', line=3, las=0, cex=0.73) 
  box()                                 # outer box
  par(oldpar)
}
## plot volume bars
plotVolumeBars <- function(X) {
  x <- 1:NROW(X)
  ## to some trickery to scale the volume: use log to the basis of thousand
  ## to find the closest basis of thousands, but ensure we pick at least 1
  ## but also ensure that we do not pick higher than 3 (aka billions)
  volDivisor <- max(1, min(3, round(log(mean(X$Volume))/log(1e3))))
  ## for 1, 2, or 3, pick the power of thousand (for display purposes)
  volDivText <- switch(volDivisor,
                       '(thousands)',
                       '(millions)',
                       '(billions)')
  Vol <- X$Volume/1e3^volDivisor        # scale volume down
  oldpar <- par(mar=c(0,4,0,4),         # no top spacing
                lend="square")          # square line ends 
  plot.new()                            # empty plot
  plot.window(range(x), range(X$scaledvol), xaxs="i")     # set up coordinates
  grid()                                # dashed grid
  segments(x, 0, x, X$scaledvol, col="blue", lwd=1)
  abline(h=100, lty='dotted', col='red')
  title(ylab="Norm. Volume", col.lab='blue')
  axis(2, col.axis='blue')              # y-axis
  par(new=TRUE)                         # add to the plot
  plot(x, Vol, col='darkgray', type='l', ylab="", axes=FALSE, xaxs="i")
  axis(4, col.axis='darkgray')
  ## need mtext() to annotate 2nd y-axis as title() doesn't do it
  mtext(paste("Volume", volDivText),
        side=4, col='darkgray', line=3, las=0, cex=0.73) 
  box()                                 # outer box
  lab.ind <- seq(1, NROW(X), length=6)
  axis(1, at=lab.ind, lab=rownames(X)[lab.ind])
  par(oldpar)
}

computeBollingerBands <- function(X, n, b){
  a.len<- nrow(X)
  MB <- rep(0,a.len)
  UP <- rep(0,a.len)
  DN <- rep(0,a.len)
  for (i in n:a.len){
    temp <- X$close[i-n+1:i]
    std <- var(temp)/n
    MB[i] <- mean(temp)
    UP[i] <- MB[i] + b*std
    DN[i] <- MB[i] - b*std
  }
  X$bbmiddle <- MB
  X$bbupper <- UP
  X$bblower <- DN
  return(X)
}


BollingerBands <- function(filename) {
  file.path <- paste("C:/Users/Administrator/Desktop/", filename, sep = "")
  X <- read.csv(file.path, header = TRUE)
  useObs <- 100                         # use this many observations
  X <- computeBollingerBands(X, 20, 2)
  X <- X[(NROW(X)-useObs):NROW(X),]     # limit to recent useObs obs.
  ## layout is a fairly user-unfriendly function for chart layout that is
  ## being replaced by grid and gridBase -- but I'm still more familiar
  ## with this one. The following says have two plots plotted in the 
  ## order 1 and 2, one on top of the other, with 75% and 25% of the spave
  layout(matrix(c(1,2,3),3,1,byrow=TRUE),
         height=c(0.75,0.125,0.125), width=1)
  ## set 'global' plot parameters: horizontal y-axis labels, tighter spacing
  ## and no outer spacing
  oldpar <- par(las=1, mar=c(2,4,2,4), oma=c(2.5,0.5,1.5,0.5))
  plotBollingerBars(X)
  mtext(paste(instrument, ": Bollinger Bars, Bands, Indicators and ",
              "normalized and absolute Volume", sep=""),
        3, outer=FALSE, line=1, cex=1.0, font=2)
  plotBollingerIndicators(X)
  plotVolumeBars(X)
  par(oldpar)           # restore graphics parameters
  invisible(X)           
}

## test

X <- BollingerBands("sz300274_10.csv")             