########################################################################################
##                        Imports and set working directory                           ##
########################################################################################
require(tseries)
require(urca) #Used for the ADF Test
require(PerformanceAnalytics)

##Change this to match where you stored the csv files folder name FullList
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\QuantInsti-Final-Project-Statistical-Arbitrage\\database\\FullList")

########################################################################################
##                                    Functions                                       ##
########################################################################################
#calculates the pair ratio and adds columns to the dataframe that will be needed later
PrepareData <- function(csvData){
  #Calculate the Pair Ratio
  csvData$pairRatio  <-  csvData[,2] / csvData[,3]
  
  #Add columns to the DF
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

#Calculate mean, stdDev, and z-score for the given Row [end]
GenerateRowValue <- function(begin, end, csvData){
  average <- mean(csvData$spread[begin:end])
  stdev <- sd(csvData$spread[begin:end])
  
  csvData$mean[end] <-  average
  csvData$stdev[end] <- stdev
  csvData$zScore[end] <- (csvData$spread[end]-average)/stdev
  
  return(csvData)
  
}

#Generate trading signals based on a z-score of 1 and -1
GenerateSignal <- function(counter, csvData){
  #Trigger and close represent the entry and exit zones (value refers to the z-score value)
  trigger  <- 1
  close  <-  0.5
  
  currentSignal <- csvData$signal[counter]
  prevSignal <- csvData$signal[counter-1]
  
  #Set trading signal for the given [end] row
  if(csvData$adfTest[counter] == 1)
  {
    #If there is a change in signal from long to short then you must allow for the 
    #current trade to first be closed
    if(currentSignal == -1 && prevSignal == 1)
      csvData$signal[counter] <- 0
    else if(currentSignal == 1 && prevSignal == -1)
      csvData$signal[counter] <- 0
    
    #Create a long / short signal if the current z-score is larger / smaller than the trigger value
    #(respectively)
    else if(csvData$zScore[counter] > trigger)
      csvData$signal[counter] <- -1
    else if (csvData$zScore[counter] < -trigger)
      csvData$signal[counter] <- 1
    
    #Close the position if z-score is beteween the two "close" values
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
  #In a pair trading strategy you need to go long one share and short the other
  #and then reverse the transaction when you close
  
  ##First Leg of the trade (Set Long position)
  #If there is no change in signal
  if(currentSignal == 0 && prevSignal == 0)
    csvData$BuyPrice[end] <- 0    
  else if(currentSignal == prevSignal)
    csvData$BuyPrice[end] <- csvData$BuyPrice[end-1]     
  
  #If the signals point to a new trade
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
  else if(currentSignal == 0 && prevSignal == -1)
    csvData$BuyPrice[end] <- csvData[end, 3] * transactionPairRatio  
  
  
  
  ##Second Leg of the trade (Set Short position)
  ##Set Short Prices if there is no change in signal
  if(currentSignal == 0 && prevSignal == 0)
    csvData$SellPrice[end] <- 0    
  else if(currentSignal == prevSignal)
    csvData$SellPrice[end] <- csvData$SellPrice[end-1] 
  
  #If the signals point to a new trade
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
  #Calculate the returns generated on each leg of the deal (the long and the short position)
  #Long leg of the trade
  if(csvData$signal[end] == 0 && csvData$signal[end-1] != 0 )
    csvData$LongReturn[end] <- (csvData$BuyPrice[end] / csvData$BuyPrice[end-1]) - 1
  #Short Leg of the trade
  if(csvData$signal[end] == 0 && csvData$signal[end-1] != 0 )
    csvData$ShortReturn[end] <- (csvData$SellPrice[end-1] / csvData$SellPrice[end]) - 1
  
  #Add slippage
  if(csvData$ShortReturn[end] != 0)
    csvData$Slippage[end] <- slippage
  
  #If a trade was closed then calculate the total return
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
  
  #loop through the data to find the + & - trades and total trades
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
  
  #Print the results to the console
  print(paste("Total Trades: ", totalTrades))
  print(paste("Success Rate: ", positiveTrades/totalTrades))
  print(paste("PnL Ratio: ", mean(profitsVector)/mean(lossesVector*-1)))
  print(table.Drawdowns(returns))
  
}
#Use this one if you have the returns in xts format and want to generate a report
GenerateReport.xts <- function(returns, startDate = '2005-01-01', endDate = '2015-11-23'){
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
  
  #Itterate through data to get the + & - trades
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
  
  #Print results to Console
  print(paste("Total Trades: ", totalTrades))
  print(paste("Success Rate: ", positiveTrades/totalTrades))
  print(paste("PnL Ratio: ", mean(profitsVector)/mean(lossesVector*-1)))
  print(table.Drawdowns(returns))
  
}

#The function that will be called by the user to backtest a pair
BacktestPair <- function(pairData, mean = 35, slippage = -0.0014, startDate = '2005-01-01', endDate = '2014-11-23', generate.report = TRUE){
  # At 150 data points
  # Critical value at 1% : -3.46
  # Critical value at 5% : -2.88
  # Critical value at 10% : -2.57
  cirtValue  <- -2.58 #Critical Value used in the ADF Test
  
  #Prepare the initial dataframe by adding columns and pre calculations
  pairData <- PrepareData(pairData)
  
  #Itterate through each day in the time series
  for(i in 1:length(pairData[,2])){
    #For each day after the amount of days needed to run the ADF test
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
            #If co-integrated then set the ADFTest value to true / 1
            pairData$adfTest[end]  <-  1           
          }
        }
      }
      
      #Calculate the remainder variables needed
      if(i >= mean){
        #Generate Row values
        pairData <- GenerateRowValue(begin, end, pairData)
        #Generate the Signals
        pairData <- GenerateSignal(i, pairData)
        
        currentSignal  <-  pairData$signal[i]
        prevSignal  <-  pairData$signal[i-1]
        
        #Generate Transactions
        pairData <- GenerateTransactions(currentSignal, prevSignal, i, pairData)
        
        #Get the returns with added slippage
        pairData <- GetReturns(i, pairData, slippage)
      }
    }
  }
  
  if(generate.report == TRUE)
    GenerateReport(pairData, startDate, endDate)
  
  return(pairData)
}

#An equally weighted portfolio of shares
BacktestPortfolio  <- function(names, leverage = 1, startDate = '2005-01-01', endDate = '2015-11-23'){
  ##Itterates through all the pairs and backtests each one
  ##stores the data in a list of numerical vectors
  returns.list  <- list()
  counter  <-  F
  ticker  <- 1
  for (name in names){
    #A notification to let you know how far it is
    print(paste(ticker, " of ", length(names)))
    ticker  <- ticker + 1
    
    #Run the backtest on the pair
    data <- read.csv(name)   
    BackTest.df <- BacktestPair(data, 35, generate.report = FALSE)
    
    #Store the dates in a seperate vector
    if (counter == F){
      dates  <<- as.Date(BackTest.df$Date)
      counter  <- T
    }
    
    #Append to list
    returns.list  <- c(returns.list, list(BackTest.df[,18]))
  }
  
  ##Aggregates the returns for each day and then calculates the average for each day
  total.returns  <- c()
  for (i in 1:length(returns.list)){
    if(i == 1)
      total.returns = returns.list[[i]]
    else
      total.returns = total.returns + returns.list[[i]]
  }
  
  total.returns <- total.returns / length(returns.list)
  
  ##Generate a report for the portfolio
  returns  <-  xts(total.returns * leverage, dates)
  GenerateReport.xts(returns, startDate, endDate)
  
  return(returns)
}

#Mock test to make sure everything is running
data <- read.csv('investec.csv') 
a <- BacktestPair(data, 35)

########################################################################################
##                 Portfolio COnstruction for in sample test                          ##
########################################################################################

##############################
#   Construction companies   # 
##############################
##Use this section to test individual pairs
data <- read.csv('groupmr.csv')     
data <- read.csv('groupppc.csv')    
data <- read.csv('groupavenge.csv')
data <- read.csv('groupwhbo.csv')   
data <- read.csv('mrppc.csv')
data <- read.csv('mrwhbo.csv')      
data <- read.csv('mravenge.csv')    
data <- read.csv('ppcwhbo.csv')     
data <- read.csv('ppcavenge.csv')   

a <- BacktestPair(data, 35)


##Run this section if you want a portfolio of construction companies
names  <- c('groupmr.csv', 'groupppc.csv', 'groupavenge.csv', 'groupwhbo.csv', 
            'mrppc.csv', 'mrwhbo.csv', 'mravenge.csv', 'ppcwhbo.csv', 'ppcavenge.csv')

return.series  <- BacktestPortfolio(names, startDate = '2014-11-23', endDate = '2015-11-23')

##Run this code if you want to see the full series (from begining of data to end)
leverage <- 1
GenerateReport.xts(return.series * leverage)

##############################
#         Insurance          #
##############################
##Use this section to test individual pairs
data <- read.csv('disclib.csv')   
data <- read.csv('discmmi.csv') 
data <- read.csv('discsanlam.csv')   
data <- read.csv('libmmi.csv')    
data <- read.csv('mmiold.csv') )
data <- read.csv('mmisanlam.csv')  
data <- read.csv('oldsanlam.csv') 

a <- BacktestPair(data, 35)

##Run this section if you want a portfolio of construction companies
names  <- c('disclib.csv', 'discmmi.csv', 'discsanlam.csv', 'libmmi.csv', 'mmiold.csv',
            'mmisanlam.csv', 'oldsanlam.csv')

return.series  <- BacktestPortfolio(names, startDate = '2014-11-23', endDate = '2015-11-23')

##Run this code if you want to see the full series (from begining of data to end)
leverage <- 1
GenerateReport.xts(return.series * leverage)

##############################
#     Wireless Telecoms      #
##############################
data <- read.csv('MTNVODA.csv')   
a <- BacktestPair(data, 35, startDate = '2010-01-01', endDate = '2015-11-23')

##############################
#           Paper            #
##############################

data <- read.csv('YorkSappi.csv')   
a <- BacktestPair(data, 35)


##############################
#        fin services        # 
##############################
##Use this section to test individual pairs
data <- read.csv('saspere.csv')   
data <- read.csv('braitpere.csv') 
data <- read.csv('braitsas.csv')  
data <- read.csv('psgpere.csv')   
data <- read.csv('psgsas.csv')    
data <- read.csv('psgbrait.csv')  
data <- read.csv('corpere.csv')   
data <- read.csv('corsasfin.csv') 
data <- read.csv('corbrait.csv')  
data <- read.csv('corpsg.csv')    

a <- BacktestPair(data, 35)


##Run this section if you want a portfolio of construction companies
names  <- c('corpsg.csv', 'corbrait.csv', 'corsasfin.csv', 'corpere.csv', 'psgbrait.csv',
            'psgsas.csv', 'psgpere.csv', 'braitsas.csv', 'braitpere.csv', 'saspere.csv')

return.series  <- BacktestPortfolio(names, startDate = '2014-11-23', endDate = '2015-11-23')

##Run this code if you want to see the full series (from begining of data to end)
leverage <- 1
GenerateReport.xts(return.series * leverage)


##############################
#          Banking           # 
############################## 
data <- read.csv('absaned.csv')  
a <- BacktestPair(data, 35)
data <- read.csv('absarmb.csv')  
b <- BacktestPair(data, 35)
data <- read.csv('firstabsa.csv') 
c <- BacktestPair(data, 35)
data <- read.csv('firstned.csv') 
d <- BacktestPair(data, 35)
data <- read.csv('firstrmb.csv') 
e <- BacktestPair(data, 35)
data <- read.csv('nedrmb.csv')   
f <- BacktestPair(data, 35)
data <- read.csv('sbkabsa.csv')  
g <- BacktestPair(data, 35)
data <- read.csv('sbkfirst.csv') 
h <- BacktestPair(data, 35)
data <- read.csv('sbkned.csv')   
i <- BacktestPair(data, 35)
data <- read.csv('sbkrmb.csv')   
j <- BacktestPair(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18] + i[,18] + j[,18]
answer <- answer / 10

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#        Gen Retail          # 
##############################
data <- read.csv('MRTFG.csv')    
a <- BacktestPair(data, 35)
data <- read.csv('trutfg.csv')   
b <- BacktestPair(data, 35)
data <- read.csv('trumr.csv')    
c <- BacktestPair(data, 35)
data <- read.csv('wooltfg.csv')  
d <- BacktestPair(data, 35)
data <- read.csv('woolmr.csv')   
e <- BacktestPair(data, 35)
data <- read.csv('wooltru.csv')  
f <- BacktestPair(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] +f[,18] 
answer <- answer / 6

returns  <-  xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#           Mining           # 
##############################
##Very few are profitable in the sample space
data <- read.csv('anglogoldamerican.csv') 
a <- BacktestPair(data, 35)
data <- read.csv('anglogoldplat.csv') 
b <- BacktestPair(data, 35)
data <- read.csv('angloplatamerican.csv') 
c <- BacktestPair(data, 35)
data <- read.csv('bhpanglo.csv') 
d <- BacktestPair(data, 35)
data <- read.csv('gfianglo.csv') 
e <- BacktestPair(data, 35)
data <- read.csv('haranglo.csv') 
f <- BacktestPair(data, 35)
data <- read.csv('hargfi.csv')   
g <- BacktestPair(data, 35)
data <- read.csv('impanglo.csv') 
h <- BacktestPair(data, 35)


answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18]
answer <- answer / 8

returns = xts(answer, as.Date(a$Date))
charts.PerformanceSummary(returns)

##############################
#         Stat Arb           #
##############################
data <- read.csv('investec.csv') 
investec <- BacktestPair(data, 35) 

data <- read.csv('mondi.csv') 
mondi <- BacktestPair(data, 35) 


########################################################################################
##                 Portfolio COnstruction for out-of-sample test                      ##
########################################################################################
##Out of sample test with an equaly weighted portfolio

##Final Out of sample test for 2015 year
startDate  <-  '2014-11-01'
endDate  <- '2015-11-23'

#StatArb
data <- read.csv('investec.csv') 
a <- BacktestPair(data, 35) 

#Mining
data <- read.csv('angloplatamerican.csv') 
b <- BacktestPair(data, 35)
data <- read.csv('gfianglo.csv') 
c <- BacktestPair(data, 35)
data <- read.csv('impanglo.csv') 
d <- BacktestPair(data, 35)

#Gen Retail
data <- read.csv('MRTFG.csv')    
e <- BacktestPair(data, 35)
data <- read.csv('trutfg.csv')   
f <- BacktestPair(data, 35)
data <- read.csv('trumr.csv')    
g <- BacktestPair(data, 35)
data <- read.csv('wooltru.csv')  
h <- BacktestPair(data, 35)

#Banks
data <- read.csv('absaned.csv')  
i <- BacktestPair(data, 35)
data <- read.csv('absarmb.csv')  
j <- BacktestPair(data, 35)
data <- read.csv('firstabsa.csv') 
k <- BacktestPair(data, 35)
data <- read.csv('firstned.csv') 
l <- BacktestPair(data, 35)
data <- read.csv('firstrmb.csv') 
m <- BacktestPair(data, 35)
data <- read.csv('sbkabsa.csv')  
n <- BacktestPair(data, 35)
data <- read.csv('sbkfirst.csv') 
o <- BacktestPair(data, 35)
data <- read.csv('sbkned.csv')   
p <- BacktestPair(data, 35)
data <- read.csv('sbkrmb.csv')   
q <- BacktestPair(data, 35)

#Fin Services
data <- read.csv('braitsas.csv')  
r <- BacktestPair(data, 35)
data <- read.csv('corpere.csv')   
s <- BacktestPair(data, 35)

#Construction
data <- read.csv('groupppc.csv')    
t <- BacktestPair(data, 35)
data <- read.csv('groupavenge.csv') 
u <- BacktestPair(data, 35)
data <- read.csv('groupwhbo.csv')   
v <- BacktestPair(data, 35)
data <- read.csv('mrppc.csv')       
w <- BacktestPair(data, 35)
data <- read.csv('mrwhbo.csv')      
x <- BacktestPair(data, 35)
data <- read.csv('mravenge.csv')    
y <- BacktestPair(data, 35)
data <- read.csv('ppcwhbo.csv')     
z <- BacktestPair(data, 35)
data <- read.csv('ppcavenge.csv') 
aa <- BacktestPair(data, 35)

answer <- a[,18] + b[,18] + c[,18] + d[,18] + e[,18] + f[,18] + g[,18] + h[,18] + i[,18] + j[,18] + k[,18] + l[,18] + m[,18] + n[,18] + o[,18] + p[,18] + q[,18] + r[,18] + s[,18] + t[,18] + u[,18] + v[,18] + w[,18] + x[,18] + y[,18] + z[,18] + aa[,18]
answer <- answer / 27

#Subset the date to the out of sample dates selected
returns = xts(answer*5, as.Date(a$Date))
GenerateReport.xts(returns, '2005-01-01','2015-11-23')
