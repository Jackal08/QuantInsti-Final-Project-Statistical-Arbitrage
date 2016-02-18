########################################################################################
##                        Imports and set working directory                           ##
########################################################################################
require(tseries)
require(urca)
require(PerformanceAnalytics)

##Change this to match where you stored the csv files
setwd("C:/Users/Administrator/Desktop/QUantInsti Project/database/fulllist")

########################################################################################
##                                    Functions                                       ##
########################################################################################
#calculates the pair ratio and adds columns to the dataframe that will be needed later
PrepareData <- function(csvData){
  #Calculate the Pair Ratio
  csvData$pairRatio  <-  csvData[,2] / csvData[,3]
  
  #Add the columns
  csvData <- AddColumns(csvData)
  
  #Make sure that the date column is not read in as a vector of characters
  csvData$Date <- as.Date(csvData$Date)
  
  return(csvData)
}
AddColumns <- function(csvData){
  #Calculate the log prices of the two time series
  csvData$LogA <- log10(csvData[,2])
  csvData$LogB <- log10(csvData[,3])
  
  #Add Columns to csvDataframe
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

#Calculate mean, stdDev, z-score, 
GenerateRowValue <- function(begin, end, csvData){
  average <- mean(csvData$spread[begin:end])
  stdev <- sd(csvData$spread[begin:end])
  
  csvData$mean[end] <-  average
  csvData$stdev[end] <- stdev
  csvData$zScore[end] <- (csvData$spread[end]-average)/stdev
  
  return(csvData)
  
}

#Generate trading signals based on a z-score of 2 and -2
GenerateSignal <- function(counter, csvData){
  trigger  <- 1
  close  <-  0.5
  
  currentSignal <- csvData$signal[counter]
  prevSignal <- csvData$signal[counter-1]
  
  if(csvData$adfTest[counter] == 1)
  {
    if(currentSignal == -1 && prevSignal == 1)
      csvData$signal[counter] <- 0
    else if(currentSignal == 1 && prevSignal == -1)
      csvData$signal[counter] <- 0
    
    else if(csvData$zScore[counter] > trigger)
      csvData$signal[counter] <- -1
    else if (csvData$zScore[counter] < -trigger)
      csvData$signal[counter] <- 1
    
    else if (csvData$zScore[counter] < close && csvData$zScore[counter] > -close)
      csvData$signal[counter] <- 0
    else 
      csvData$signal[counter] <- prevSignal
  }
  else 
    csvData$signal[counter] <- 0
  
  return(csvData)
}

#Transactions based on trade signal
#Following the framework set out initially by QuantInsti (Note: this can be coded better) 
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

#Calculate the returns generated after each transaction
#Add implementation shortfall / slippage
GetReturns <- function(end, csvData, slippage){
  if(csvData$signal[end] == 0 && csvData$signal[end-1] != 0 )
    csvData$LongReturn[end] <- (csvData$BuyPrice[end] / csvData$BuyPrice[end-1]) - 1
  
  if(csvData$signal[end] == 0 && csvData$signal[end-1] != 0 )
    csvData$ShortReturn[end] <- (csvData$SellPrice[end-1] / csvData$SellPrice[end]) - 1
  
  #Add slippage
  if(csvData$ShortReturn[end] != 0)
    csvData$Slippage[end] <- slippage
  
  if(csvData$ShortReturn[end] != 0 && csvData$LongReturn[end] != 0)
    csvData$TotalReturn[end] <- ((csvData$ShortReturn[end] + csvData$LongReturn[end]) / 2) + csvData$Slippage[end]
  
  return(csvData)
}

#Returns an equity curve, annualized return, annualized sharpe ratio, and max drawdown
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
GenerateReport.xts <- function(returns, startDate, endDate){
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

#The function that will be called by the user
pairTradeBacktest <- function(pairData, mean = 35, slippage = -0.0014, startDate = '2005-01-01', endDate = '2014-11-23'){
  # At 150 data points
  # Critical value at 1% : -3.46
  # Critical value at 5% : -2.88
  # Critical value at 10% : -2.57
  cirtValue  <- -2.58
  
  #Prepare the initial dataframe by adding columns and pre calculations
  pairData <- PrepareData(pairData)
  
  #Itterate through each day in the time series
  for(i in 1:length(pairData[,2])){
    if(i > 130){
      begin  <-  i - mean + 1
      end  <-  i
      
      #Calculate Spread
      spread  <-  pairData$pairRatio[end]
      pairData$spread[end]  <-  spread
      
      #ADF Test 
      #120 - 90 - 60 
      if(adf.test(pairData$spread[(i-120):end], k = 1)[1] <= cirtValue){
        if(adf.test(pairData$spread[(i-90):end], k = 1)[1] <= cirtValue){
          if(adf.test(pairData$spread[(i-60):end], k = 1)[1] <= cirtValue){
            pairData$adfTest[end]  <-  1           
          }
        }
      }
      
      #Calculate the remainder variables
      if(i >= mean){
        pairData <- GenerateRowValue(begin, end, pairData)
        pairData <- GenerateSignal(i, pairData)
        
        currentSignal  <-  pairData$signal[i]
        prevSignal  <-  pairData$signal[i-1]
        
        pairData <- GenerateTransactions(currentSignal, prevSignal, i, pairData)
        
        pairData <- GetReturns(i, pairData, slippage)
      }
    }
  }
  
  GenerateReport(pairData, startDate, endDate)
  
  return(pairData)
}

data <- read.csv('investec.csv') 
a <- pairTradeBacktest(data, 35) #Yes

########################################################################################
##                 Portfolio COnstruction for in sample test                          ##
########################################################################################

##############################
#   Construction companies   # No
##############################
data <- read.csv('groupmr.csv')     #No
a <- pairTradeBacktest(data, 35)
data <- read.csv('groupppc.csv')    #Yes
b <- pairTradeBacktest(data, 35)
data <- read.csv('groupavenge.csv') #Yes
c <- pairTradeBacktest(data, 35)
data <- read.csv('groupwhbo.csv')   #Yes
d <- pairTradeBacktest(data, 35)
data <- read.csv('mrppc.csv')       #Yes
e <- pairTradeBacktest(data, 35)
data <- read.csv('mrwhbo.csv')      #Yes
f <- pairTradeBacktest(data, 35)
data <- read.csv('mravenge.csv')    #Yes
g <- pairTradeBacktest(data, 35)
data <- read.csv('ppcwhbo.csv')     #Yes
h <- pairTradeBacktest(data, 35)
data <- read.csv('ppcavenge.csv')   #No
i <- pairTradeBacktest(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18] + i[,18]
answer <- answer / 9

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#         Insurance          #
##############################
data <- read.csv('disclib.csv')   #
a <- pairTradeBacktest(data, 35)
data <- read.csv('discmmi.csv') #
b <- pairTradeBacktest(data, 35)
data <- read.csv('discsanlam.csv')   #
c <- pairTradeBacktest(data, 35)
data <- read.csv('libmmi.csv')    #
d <- pairTradeBacktest(data, 35)
data <- read.csv('mmiold.csv') #
e <- pairTradeBacktest(data, 35)
data <- read.csv('mmisanlam.csv')  #
f <- pairTradeBacktest(data, 35)
data <- read.csv('oldsanlam.csv')    #
g <- pairTradeBacktest(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18]
answer <- answer / 7

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#     Wireless Telecoms      #
##############################
data <- read.csv('MTNVODA.csv')   
a <- pairTradeBacktest(data, 35, startDate = '2010-01-01', endDate = '2015-11-23')


answer <- a[,18]

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#           Paper            #
##############################

data <- read.csv('YorkSappi.csv')   
a <- pairTradeBacktest(data, 35)


answer <- a[,18]

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#        fin services        # No
##############################
#very few candidates would be selected for the out of sample test
data <- read.csv('saspere.csv')   #No
a <- pairTradeBacktest(data, 35)
data <- read.csv('braitpere.csv') #No
b <- pairTradeBacktest(data, 35)
data <- read.csv('braitsas.csv')  #Yes
c <- pairTradeBacktest(data, 35)
data <- read.csv('psgpere.csv')   #No
d <- pairTradeBacktest(data, 35)
data <- read.csv('psgsas.csv')    #No
e <- pairTradeBacktest(data, 35)
data <- read.csv('psgbrait.csv')  #No
f <- pairTradeBacktest(data, 35)
data <- read.csv('corpere.csv')   #Yes
g <- pairTradeBacktest(data, 35)
data <- read.csv('corsasfin.csv') #No
h <- pairTradeBacktest(data, 35)
data <- read.csv('corbrait.csv')  #No
i <- pairTradeBacktest(data, 35)
data <- read.csv('corpsg.csv')    #No
j <- pairTradeBacktest(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18] + i[,18] + j[,18]
answer <- answer / 10

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)


##############################
#          Banking           # No
############################## 
data <- read.csv('absaned.csv')  #Yes
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

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18] + i[,18] + j[,18]
answer <- answer / 10

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#        Gen Retail          # No
##############################
data <- read.csv('MRTFG.csv')    #Yes
a <- pairTradeBacktest(data, 35)
data <- read.csv('trutfg.csv')   #Yes
b <- pairTradeBacktest(data, 35)
data <- read.csv('trumr.csv')    #no
c <- pairTradeBacktest(data, 35)
data <- read.csv('wooltfg.csv')  #
d <- pairTradeBacktest(data, 35)
data <- read.csv('woolmr.csv')   #No
e <- pairTradeBacktest(data, 35)
data <- read.csv('wooltru.csv')  #Yes
f <- pairTradeBacktest(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] +f[,18] 
answer <- answer / 6

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#           Mining           # No
##############################
##Very few are profitable in the sample space
data <- read.csv('anglogoldamerican.csv') #No
a <- pairTradeBacktest(data, 35)
data <- read.csv('anglogoldplat.csv') #No
b <- pairTradeBacktest(data, 35)
data <- read.csv('angloplatamerican.csv') #Yes
c <- pairTradeBacktest(data, 35)
data <- read.csv('bhpanglo.csv') #No
d <- pairTradeBacktest(data, 35)
data <- read.csv('gfianglo.csv') #Yes
e <- pairTradeBacktest(data, 35)
data <- read.csv('haranglo.csv') #No
f <- pairTradeBacktest(data, 35)
data <- read.csv('hargfi.csv')   #No
g <- pairTradeBacktest(data, 35)
data <- read.csv('impanglo.csv') #Yes
h <- pairTradeBacktest(data, 35)


answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18]
answer <- answer / 8

returns = xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#         Stat Arb           #
##############################
data <- read.csv('investec.csv') 
investec <- pairTradeBacktest(data, 35) #Yes

data <- read.csv('mondi.csv') 
mondi <- pairTradeBacktest(data, 35) #Yes



answer <- (a[,18] + b[,18])/2

answer <- a[,18]
returnsLeverage = xts(answer*4, as.Date(a$Date))
returnsStandard = xts(answer, as.Date(a$Date))


GenerateReport.xts(returnsStandard, '2008-01-01', '2013-01-01')
GenerateReport.xts(returnsLeverage, '2008-01-01', '2013-01-01')

GenerateReport.xts(returnsStandard, '2013-01-02', '2015-11-23')
GenerateReport.xts(returnsLeverage, '2013-01-02', '2015-11-23')

data <- read.csv('wescza.csv') 
investec <- pairTradeBacktest(data, 35) #Yes

########################################################################################
##                 Portfolio COnstruction for out-of-sample test                      ##
########################################################################################
##Out of sample test with an equaly weighted portfolio

##Final Out of sample test for 2015 year
startDate  <-  '2014-11-01'
endDate  <- '2015-11-23'

#StatArb
data <- read.csv('investec.csv') 
a <- pairTradeBacktest(data, 35) #Yes

#Mining
data <- read.csv('angloplatamerican.csv') #Yes
b <- pairTradeBacktest(data, 35)
data <- read.csv('gfianglo.csv') #Yes
c <- pairTradeBacktest(data, 35)
data <- read.csv('impanglo.csv') #Yes
d <- pairTradeBacktest(data, 35)

#Gen Retail
data <- read.csv('MRTFG.csv')    #Yes
e <- pairTradeBacktest(data, 35)
data <- read.csv('trutfg.csv')   #Yes
f <- pairTradeBacktest(data, 35)
data <- read.csv('trumr.csv')    #Yes
g <- pairTradeBacktest(data, 35)
data <- read.csv('wooltru.csv')  #Yes
h <- pairTradeBacktest(data, 35)

#Banks
data <- read.csv('absaned.csv')  #Yes
i <- pairTradeBacktest(data, 35)
data <- read.csv('absarmb.csv')  #Yes
j <- pairTradeBacktest(data, 35)
data <- read.csv('firstabsa.csv') #Yes
k <- pairTradeBacktest(data, 35)
data <- read.csv('firstned.csv') #Yes
l <- pairTradeBacktest(data, 35)
data <- read.csv('firstrmb.csv') #Yes
m <- pairTradeBacktest(data, 35)
data <- read.csv('sbkabsa.csv')  #Yes
n <- pairTradeBacktest(data, 35)
data <- read.csv('sbkfirst.csv') #YEs
o <- pairTradeBacktest(data, 35)
data <- read.csv('sbkned.csv')   #Yes
p <- pairTradeBacktest(data, 35)
data <- read.csv('sbkrmb.csv')   #Yes
q <- pairTradeBacktest(data, 35)

#Fin Services
data <- read.csv('braitsas.csv')  #Yes
r <- pairTradeBacktest(data, 35)
data <- read.csv('corpere.csv')   #Yes
s <- pairTradeBacktest(data, 35)

#Construction
data <- read.csv('groupppc.csv')    #Yes
t <- pairTradeBacktest(data, 35)
data <- read.csv('groupavenge.csv') #Yes
u <- pairTradeBacktest(data, 35)
data <- read.csv('groupwhbo.csv')   #Yes
v <- pairTradeBacktest(data, 35)
data <- read.csv('mrppc.csv')       #Yes
w <- pairTradeBacktest(data, 35)
data <- read.csv('mrwhbo.csv')      #Yes
x <- pairTradeBacktest(data, 35)
data <- read.csv('mravenge.csv')    #Yes
y <- pairTradeBacktest(data, 35)
data <- read.csv('ppcwhbo.csv')     #Yes
z <- pairTradeBacktest(data, 35)
data <- read.csv('ppcavenge.csv')   #Yes
aa <- pairTradeBacktest(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18] + i[,18] + j[,18] + k[,18] + l[,18] + m[,18] + n[,18] + o[,18] + p[,18] + q[,18] + r[,18] + s[,18] + t[,18] + u[,18] + v[,18] + w[,18] + x[,18] + y[,18] + z[,18] + aa[,18]
answer <- answer / 27

#Subset the date to the out of sample dates selected
returns = xts(answer*5, as.Date(a$Date))
GenerateReport.xts(returns, '2005-01-01','2015-11-23')
