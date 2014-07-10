#calculation linear correlation on return


GetStockReturn = function(filename,startdate,enddate) {
  stock.dataframe = read.csv(paste(getwd(),"/../data/",filename,sep=""),header = TRUE, stringsAsFactors = FALSE)  
  stock.dataframe$Date = as.Date(stock.dataframe$Date,"%Y-%m-%d")
  #data filtering
  stock.dataframe = stock.dataframe[stock.dataframe$Date>=startdate & stock.dataframe$Date<=enddate,]
  #data sorting
  stock.dataframe = stock.dataframe[order(as.numeric(stock.dataframe$Date)),]
  
  #stock.dataframe
  
  #creating returns dataframe
  StockReturns = data.frame(Date = as.Date(rep(0,nrow(stock.dataframe)-1), origin = "1900-01-01"),
                            Return = rep(NA,nrow(stock.dataframe)-1))
  
  for (i in seq(2,nrow(stock.dataframe))) {
    StockReturns$Date[i-1] = stock.dataframe$Date[i]
    StockReturns$Return[i-1] = log(stock.dataframe$Adj.Close[i]/stock.dataframe$Adj.Close[i-1])
  }
  return(StockReturns)
}


BMY_Returns = GetStockReturn("stock_BMY.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
TMSNRC_Returns = GetStockReturn("stock_TMSNRC.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
HPQ_Returns = GetStockReturn("stock_HPQ.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
IBM_Returns = GetStockReturn("stock_IBM.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
PFE_Returns = GetStockReturn("stock_PFE.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))

#(BMY_Returns$Date == RSH_Returns$Date)
#(BMY_Returns$Date == HPQ_Returns$Date)
#(BMY_Returns$Date == IBM_Returns$Date)
#(BMY_Returns$Date == PFE_Returns$Date)

#plot(density(BMY_Returns$Return),main="Stock log returns",ylab="",xlab="", col="black",lty=1)


returns_linear_correlation_match_dates = function(R1,R2) {
  cat("R1: ", nrow(R1), " return(s)\n")
  cat("R2: ", nrow(R2), " return(s)\n")  
  intersection = intersect(R1$Date,R2$Date)
  cat("Intersect(R1,R2): ", length(intersect(R1$Date,R2$Date)), " return(s)\n")
  
  R1_filtered = rep(NA,length(intersection))
  R2_filtered = rep(NA,length(intersection))
  
  for (i in seq(1,length(intersection))) {
    R1_filtered[i] = R1[R1$Date==intersection[i],]$Return
    R2_filtered[i] = R2[R2$Date==intersection[i],]$Return
  }
  
  return(cor(R1_filtered,R2_filtered,method = "pearson"))
}

myReturnMatrix = cbind(BMY_Returns$Return,TMSNRC_Returns$Return,HPQ_Returns$Return,IBM_Returns$Return,PFE_Returns$Return)

#Correlation of stock returns
ReturnCorrelationMatrix = matrix(NA, 
              nrow=ncol(myReturnMatrix),
              ncol=ncol(myReturnMatrix),
              byrow = TRUE);

rownames(ReturnCorrelationMatrix) = AssetTicker
colnames(ReturnCorrelationMatrix) = AssetTicker 

for (i in seq(1,ncol(myReturnMatrix))) {
  for (j in seq(1,ncol(myReturnMatrix))) {
    #cat("i/j:",i,"-",j,"----->",cor(myReturnMatrix[,i],myReturnMatrix[,j],method = "pearson"),"\n")
    ReturnCorrelationMatrix[i,j] = cor(myReturnMatrix[,i],myReturnMatrix[,j],method = "pearson")
    #ReturnCorrelationMatrix[i,j] = returns_linear_correlation_match_dates(myReturnMatrix[i,],myReturnMatrix[j,])
  }
}

ReturnCorrelationMatrix

#----------------------------------------------------------------------------------
#Rolling 60days correlation (stock log return
#----------------------------------------------------------------------------------



lag = 60
rolling_correlation_1vs5 = rep(NA,length(BMY_Returns$Return)-lag)
rolling_correlation_3vs4 = rep(NA,length(BMY_Returns$Return)-lag)
asset1 = BMY_Returns$Return
asset2 = TMSNRC_Returns$Return
asset3 = HPQ_Returns$Return
asset4 = IBM_Returns$Return
asset5 = PFE_Returns$Return
for (i in seq(1,length(rolling_correlation_1vs5))) {
  rolling_correlation_1vs5[i] = pearson_correlation(asset1[i:(i+(lag-1))],asset5[i:(i+(lag-1))])
  rolling_correlation_3vs4[i] = pearson_correlation(asset3[i:(i+(lag-1))],asset4[i:(i+(lag-1))])
}
matplot(seq(1,length(rolling_correlation_1vs5)),cbind(rolling_correlation_1vs5,rolling_correlation_3vs4),type="l",col=c("black","blue"),lty=1,xlab="Time",ylab="Correlation",ylim=c(0,0.75))
legend(146, 0.14, c("Corr(BMY,PFE)","Corr(HP,IBM)"), col = c("black","blue"), text.col = "black", lty = c(1,1), pch = c(NA),
       merge = TRUE, bg = "gray90")


