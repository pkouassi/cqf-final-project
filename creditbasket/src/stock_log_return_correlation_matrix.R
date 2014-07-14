# calculation correlation matrix using stock log return

# Get stock prices from Yahoo csv file and calculate daily log return
GetStockLogReturn = function(filename,startdate,enddate) {
  stock.dataframe = read.csv(paste(getwd(),"/../data/",filename,sep=""),header = TRUE, stringsAsFactors = FALSE)  
  stock.dataframe$Date = as.Date(stock.dataframe$Date,"%Y-%m-%d")
  # data filtering
  stock.dataframe = stock.dataframe[stock.dataframe$Date>=startdate & stock.dataframe$Date<=enddate,]
  # data sorting
  stock.dataframe = stock.dataframe[order(as.numeric(stock.dataframe$Date)),]
  
  # creating returns dataframe
  StockReturns = data.frame(Date = as.Date(rep(0,nrow(stock.dataframe)-1), origin = "1900-01-01"),
                            Return = rep(NA,nrow(stock.dataframe)-1))
  
  for (i in seq(2,nrow(stock.dataframe))) {
    StockReturns$Date[i-1] = stock.dataframe$Date[i]
    StockReturns$Return[i-1] = log(stock.dataframe$Adj.Close[i]/stock.dataframe$Adj.Close[i-1])
  }
  return(StockReturns)
}

# Get stock log return for each of the reference name
Stock1 = GetStockLogReturn("stock_BMY.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
Stock2 = GetStockLogReturn("stock_TMSNRC.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
Stock3 = GetStockLogReturn("stock_HPQ.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
Stock4 = GetStockLogReturn("stock_IBM.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
Stock5 = GetStockLogReturn("stock_PFE.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))

# plot the kernel density of the stock
plot(density(Stock1$Return),main="Stock log returns",ylab="",xlab="", col="black",lty=1)


# Calculate orrelation of stock returns
myReturnMatrix = cbind(Stock1$Return,Stock2$Return,Stock3$Return,Stock4$Return,Stock5$Return)
StockLogReturnCorrelationMatrix = matrix(NA, 
              nrow=ncol(myReturnMatrix),
              ncol=ncol(myReturnMatrix),
              byrow = TRUE);

rownames(StockLogReturnCorrelationMatrix) = AssetTicker
colnames(StockLogReturnCorrelationMatrix) = AssetTicker 

for (i in seq(1,ncol(myReturnMatrix))) {
  for (j in seq(1,ncol(myReturnMatrix))) {
    StockLogReturnCorrelationMatrix[i,j] = cor(myReturnMatrix[,i],myReturnMatrix[,j],method = "pearson")
  }
}

# Calculate and display rolling 60-days correlation (stock log return)
lag = 60
rolling_correlation_1vs5 = rep(NA,length(Stock1$Return)-lag)
rolling_correlation_3vs4 = rep(NA,length(Stock1$Return)-lag)
asset1 = Stock1$Return
asset2 = Stock2$Return
asset3 = Stock3$Return
asset4 = Stock4$Return
asset5 = Stock5$Return
for (i in seq(1,length(rolling_correlation_1vs5))) {
  rolling_correlation_1vs5[i] = pearson_correlation(asset1[i:(i+(lag-1))],asset5[i:(i+(lag-1))])
  rolling_correlation_3vs4[i] = pearson_correlation(asset3[i:(i+(lag-1))],asset4[i:(i+(lag-1))])
}
matplot(seq(1,length(rolling_correlation_1vs5)),cbind(rolling_correlation_1vs5,rolling_correlation_3vs4),type="l",col=c("black","blue"),lty=1,xlab="Time",ylab="Correlation",ylim=c(0,0.75))
legend(146, 0.14, c("Corr(BMY,PFE)","Corr(HP,IBM)"), col = c("black","blue"), text.col = "black", lty = c(1,1), pch = c(NA),
       merge = TRUE, bg = "gray90")


