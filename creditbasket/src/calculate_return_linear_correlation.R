#calculation linear correlation on return


GetStockReturn = function(pathname,startdate,enddate) {

  stock.dataframe = read.csv(pathname,header = TRUE, stringsAsFactors = FALSE)  
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


IBM_Returns = GetStockReturn("P://CQF//FinalProject//git-root//finalproject//creditbasket//data//stocks//IBM.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
HPQ_Returns = GetStockReturn("P://CQF//FinalProject//git-root//finalproject//creditbasket//data//stocks//HPQ.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
PFE_Returns = GetStockReturn("P://CQF//FinalProject//git-root//finalproject//creditbasket//data//stocks//PFE.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))
BMY_Returns = GetStockReturn("P://CQF//FinalProject//git-root//finalproject//creditbasket//data//stocks//BMY.csv",as.Date("06-MAY-2013","%d-%b-%Y"),as.Date("23-MAY-2014","%d-%b-%Y"))


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

#not too good
returns_linear_correlation_match_dates(BMY_Returns,HPQ_Returns)
returns_linear_correlation_match_dates(IBM_Returns,HPQ_Returns)

#good
returns_linear_correlation_match_dates(BMY_Returns,PFE_Returns)
returns_linear_correlation_match_dates(BMY_Returns,IBM_Returns)
returns_linear_correlation_match_dates(PFE_Returns,IBM_Returns)


