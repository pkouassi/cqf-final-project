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

myReturnArray = c(BMY_Returns,RSH_Returns,HPQ_Returns,IBM_Returns,PFE_Returns)

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

