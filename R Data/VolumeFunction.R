refresh <- function(tix){
  getSymbols(colnames(tix)[-1])
}

getVolume <- function(tix) {
  #Generate list of tickers
  tickers <- colnames(tix)[-1]
  
  #Call up the data for each stock
  i = 2
  
  #Create first XTS file
  temp = get(tickers[1])
  bix <- Vo(temp)["2016-09-15/2016-12-14"]
  
  while (i<=199) {
    temp <- get(tickers[i]) #Put the temporary data for each stock in here
    tempVolume <- Vo(temp)["2016-09-15/2016-12-14"]#Get the volume for each stock
    bix <- merge(bix, tempVolume)
    i = i+1
  }
  bix <- na.omit(bix)
  bix
}

getDollarVolume <- function(tix) {
  #This function creates the dollar volume traded for a stock
  #based on the average of it's high and low prices for the day.
  
  #Generate list of tickers
  tickers <- colnames(tix)[-1]
  
  #Call up the data for each stock
  i = 2
  
  #Create first XTS file
  temp = get(tickers[1])
  bix <- Vo(temp)["2016-09-15/2016-12-14"] * ((Hi(temp)["2016-09-15/2016-12-14"] + Lo(temp)["2016-09-15/2016-12-14"] )/2)
  
  while (i<=199) {
    temp <- get(tickers[i]) #Put the temporary data for each stock in here
    tempVolume <- Vo(temp)["2016-09-15/2016-12-14"] * ((Hi(temp)["2016-09-15/2016-12-14"] + Lo(temp)["2016-09-15/2016-12-14"] )/2)#Get the volume for each stock
    bix <- merge(bix, tempVolume)
    i = i+1
  }
  bix <- na.omit(bix)
  bix
}

stockMeanVolume <- function (avol, bvol) {
  i = 1
  Average.AVolume = rep(0,199)
  meanVol <- data.frame(row.names = tickers, Average.AVolume)
  
  while(i<=199){
    meanVol [i,1]  <- mean(avol[,i])
    i = i+1
  }
  
  i = 1
  
  while(i<=199){
    meanVol [i,2]  <- mean(bvol[,i])
    i = i+1
  }
  colnames( meanVol ) <- c("A Volume", "B Volume")
  meanVol
}

graphVolumes <- function (volume) {
  i = 2
  
  plot.xts(volume[,1])
  
  while (i<=60) {
    points(volume[,i], type = 'l')
    i = i+1
  }
}

pvals <- function (avol, bvol) {
  i = 1
  pvalu <- 0
  
  while (i<=length(avol[,1])) {
    x <- t.test(avol[,i], bvol[,i], var.equal = FALSE, paired = FALSE)
    pvalu[i] <- x$p.value
    i = i+1
  }
  pvalu
}

averageWidth <- function (volume) {
  
  for (i in length(volume)) {
    
  }
}