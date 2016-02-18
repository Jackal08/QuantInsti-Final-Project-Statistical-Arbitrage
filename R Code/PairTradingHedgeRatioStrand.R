##Settings and imports
require(tseries)
require(urca)
require(PerformanceAnalytics)

##Change this to match where you stored the csv files
setwd("C:/Users/Administrator/Desktop/QUantInsti Project/database/fulllist")

##Functions 
PrepareData <- function(csvData){
  csvData$pairRatio = csvData[,2] / csvData[,3]
  csvData <- AddColumns(csvData)
  csvData$Date = as.Date(csvData$Date)
  
  return(csvData)
}
AddColumns <- function(csvData){
  #Add Columns to csvDataframe
  csvData$LogA <- log10(csvData[,2])
  csvData$LogB <- log10(csvData[,3])
  
  csvData$hedgeRatio <- 0
  csvData$spread <- 0
  csvData$adfTest <- 0
  
  csvData$mean <- 0
  csvData$stdev <- 0
  csvData$zScore <- 0
  csvData$signal <- 0
  csvData$BuyPrice <- 0
  csvData$SellPrice <- 0
  csvData$LongReturn <- 0
  csvData$ShortReturn <- 0
  csvData$Slippage <- 0
  csvData$TotalReturn <- 0
  return(csvData)
}

GenerateRowValue <- function(begin, end, csvData){
  average <- mean(csvData$spread[begin:end])
  stdev <- sd(csvData$spread[begin:end])
  csvData$mean[end] <-  average
  csvData$stdev[end] <- stdev
  csvData$zScore[end] <- (csvData$spread[end]-average)/stdev
  
  return(csvData)
  
}
GenerateSignal <- function(counter, csvData){
  currentSignal = csvData$signal[counter]
  prevSignal = csvData$signal[counter-1]
  
  if(csvData$adfTest[counter] == 1)
    {
      if(currentSignal == -1 && prevSignal == 1)
        csvData$signal[counter] <- 0
      else if(currentSignal == 1 && prevSignal == -1)
        csvData$signal[counter] <- 0    
      else if(csvData$zScore[counter] > 2)
        csvData$signal[counter] <- -1
      else if (csvData$zScore[counter] < -2)
        csvData$signal[counter] <- 1
      else if (csvData$zScore[counter] < 0.3 && csvData$zScore[counter] > -0.3)
        csvData$signal[counter] <- 0
      else 
        csvData$signal[counter] <- prevSignal
    }
  else 
    csvData$signal[counter] <- 0
  
  return(csvData)
}
GenerateTransactions <- function(currentSignal, prevSignal, end, csvData){
  #Set Long Prices
  if(currentSignal == 0 && prevSignal == 0)
    csvData$BuyPrice[end] <- 0    
  else if(currentSignal == prevSignal)
    csvData$BuyPrice[end] <- csvData$BuyPrice[end-1]     
  
  #Short B and Long A
  else if(currentSignal == 1 && currentSignal != prevSignal)
    csvData$BuyPrice[end] <- csvData[end, 2] 
  #Short A and Long B
  else if(currentSignal == -1 && currentSignal != prevSignal){
    csvData$BuyPrice[end] <- csvData[end, 3] * csvData$pairRatio[end]
    transactionPairRatio <<- csvData$pairRatio[end]
  }
  
  #Close trades
  else if(currentSignal == 0 && prevSignal == 1)
    csvData$BuyPrice[end] <- csvData[end, 2] 
  else if(currentSignal == 0 && prevSignal == -1){
    csvData$BuyPrice[end] <- csvData[end, 3] * transactionPairRatio
    
  }
  
  #Set Short Prices
  if(currentSignal == 0 && prevSignal == 0)
    csvData$SellPrice[end] <- 0    
  else if(currentSignal == prevSignal)
    csvData$SellPrice[end] <- csvData$SellPrice[end-1] 
  
  #Open Trades
  else if(currentSignal == 1 && currentSignal != prevSignal){
    csvData$SellPrice[end] <- csvData[end, 3] * csvData$pairRatio[end]
    transactionPairRatio <<- csvData$pairRatio[end]
  }
  else if(currentSignal == -1 && currentSignal != prevSignal)
    csvData$SellPrice[end] <- csvData[end, 2] 
  
  #Close trades
  else if(currentSignal == 0 && prevSignal == 1){
    csvData$SellPrice[end] <- csvData[end, 3] * transactionPairRatio
  }
  else if(currentSignal == 0 && prevSignal == -1)
    csvData$SellPrice[end] <- csvData[end, 2] 
  
  return(csvData)
}
GetReturns <- function(end, csvData, slippage){
  if(csvData$signal[end] == 0 && csvData$signal[end-1] != 0 )
    csvData$LongReturn[end] <- (csvData$BuyPrice[end] / csvData$BuyPrice[end-1]) - 1
  
  if(csvData$signal[end] == 0 && csvData$signal[end-1] != 0 )
    csvData$ShortReturn[end] <- (csvData$SellPrice[end-1] / csvData$SellPrice[end]) - 1
  
  if(csvData$ShortReturn[end] != 0)
    csvData$Slippage[end] <- slippage
  
  if(csvData$ShortReturn[end] != 0 && csvData$LongReturn[end] != 0)
    csvData$TotalReturn[end] <- ((csvData$ShortReturn[end] + csvData$LongReturn[end]) / 2) + csvData$Slippage[end]
  
  return(csvData)
}
GenerateReport <- function(pairData, startDate, endDate){
  #Subset the dates 
  returns  <-  xts(pairData$TotalReturn, as.Date(pairData$Date))
  returns  <-  returns[paste(startDate,endDate,sep="::")]
  
  #Plot
  charts.PerformanceSummary(returns)
  
  #Metrics
  print(paste("Annual Returns: ",Return.annualized(returns)))
  print(paste("Annualized Sharpe: " ,SharpeRatio.annualized(returns)))
  print(paste("Max Drawdown: ",maxDrawdown(returns)))
  
  #var returns = xts object
  totalTrades  <-  0
  positiveTrades  <-  0
  profitsVector  <- c()
  lossesVector  <- c()
  
  for(i in returns){
    if(i != 0){
      totalTrades  <- totalTrades + 1
      if(i > 0){
        positiveTrades  <- positiveTrades + 1
        profitsVector  <- c(profitsVector, i)
      }
      else if (i < 0){
        lossesVector  <- c(lossesVector, i)
      }
    }
  }
  
  print(paste("Total Trades: ", totalTrades))
  print(paste("Success Rate: ", positiveTrades/totalTrades))
  print(paste("PnL Ratio: ", mean(profitsVector)/mean(lossesVector*-1)))
  print(table.Drawdowns(returns)[,1:4])
  
}

pairTradeBacktest <- function(pairData, mean = 35, regression = 35, slippage = -0.0014, startDate = '2004-05-12', endDate = '2015-11-23'){
  criticalValue = -2.57
  
  ##Body of Code
  pairData <- PrepareData(pairData)
  #Itterate through each day in the time series
  for(i in 1:length(pairData[,2])){
    if(i >= regression && i > 160){
      begin  <-  i - mean + 1
      end  <-  i
      
      #Calculate Hedge Ratio
      linRegression <- lm(pairData$LogB[begin:end] ~ pairData$LogA[begin:end])
      hedgeRatio <- linRegression$coefficients[2]
      pairData$hedgeRatio[end] <- hedgeRatio
      
      #Calculate Spread
      spread <- pairData$LogA[end] - (pairData$hedgeRatio[end] * pairData$LogB[end]) 
      pairData$spread[end] <- spread
      
      #ADF Test
      if(adf.test(pairData$spread[(i-150):end], k = 1)[1] <= criticalValue){
        pairData$adfTest[end] <- 1           
      }
      else
        pairData$adfTest[end] <- 0
      
      #Calculate the remainder variables
      if(i >= regression + mean){
        pairData <- GenerateRowValue(begin, end, pairData)
        pairData <- GenerateSignal(i, pairData)
        
        currentSignal = pairData$signal[i]
        prevSignal = pairData$signal[i-1]
        
        pairData <- GenerateTransactions(currentSignal, prevSignal, i, pairData)
        pairData <- GetReturns(i, pairData, slippage)
      }
    }
  }
  
  #Reporting
  GenerateReport(pairData, startDate, endDate)
  
  return(pairData)
}

#Body
data <- read.csv('investec.csv')  #Yes
a <- pairTradeBacktest(data, 35)
data <- read.csv('absarmb.csv')  #Yes
b <- pairTradeBacktest(data, 35)
data <- read.csv('firstabsa.csv') #Yes
c <- pairTradeBacktest(data, 35)
data <- read.csv('firstned.csv') #Yes
d <- pairTradeBacktest(data, 35)
data <- read.csv('firstrmb.csv') #Yes
e <- pairTradeBacktest(data, 35)
data <- read.csv('nedrmb.csv')   #No
f <- pairTradeBacktest(data, 35)
data <- read.csv('sbkabsa.csv')  #Yes
g <- pairTradeBacktest(data, 35)
data <- read.csv('sbkfirst.csv') #No
h <- pairTradeBacktest(data, 35)
data <- read.csv('sbkned.csv')   #Yes
i <- pairTradeBacktest(data, 35)
data <- read.csv('sbkrmb.csv')   #Yes
j <- pairTradeBacktest(data, 35)

answer <- a[,19] + b[,19] + c[,19] + d[,19] + e[,19] + f[,19] + g[,19] + h[,19] + i[,19] + j[,19]
answer <- answer / 10

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)