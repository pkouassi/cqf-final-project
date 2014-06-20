parseHistoricCreditData = function(pathname,ticker,ccy,docclause) {
  files = list.files(path=pathname, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  
  MarketData = as.data.frame(matrix(ncol=11, nrow=length(files)))
  names(MarketData) = c("Date", "Ticker","ShortName", "Tier","DocClause","Spread1y","Spread2y","Spread3y","Spread4y","Spread5y","AvRating")
  
  date_format = "%d-%b-%y"  
  error_count=0
  i=1
  for (file in files) {
    tmp = read.csv(file,skip=1,header = TRUE, stringsAsFactors = FALSE)    
    index = which((tmp$Ticker == ticker & tmp$Ccy == ccy & tmp$DocClause == docclause))
    if (length(index) == 1) {
      MarketData$Date[i] = as.Date(tmp[index,]$Date,date_format)        
      MarketData$Ticker[i] = tmp[index,]$Ticker
      MarketData$ShortName[i] = tmp[index,]$ShortName
      MarketData$Tier[i] = tmp[index,]$Tier
      MarketData$DocClause[i] = tmp[index,]$DocClause
      MarketData$Spread1y[i] = as.numeric(gsub("%", "", tmp[index,]$Spread1y))/100 #spreads are expressed in percentage
      MarketData$Spread2y[i] = as.numeric(gsub("%", "", tmp[index,]$Spread2y))/100 #spreads are expressed in percentage
      MarketData$Spread3y[i] = as.numeric(gsub("%", "", tmp[index,]$Spread3y))/100 #spreads are expressed in percentage
      MarketData$Spread4y[i] = as.numeric(gsub("%", "", tmp[index,]$Spread4y))/100 #spreads are expressed in percentage
      MarketData$Spread5y[i] = as.numeric(gsub("%", "", tmp[index,]$Spread5y))/100 #spreads are expressed in percentage
      MarketData$AvRating[i] = tmp[index,]$AvRating      
      i = i + 1
    }
    else if (length(index) == 0) {
      cat("[",ticker,"] No data in file ",file,"\n")
      error_count = error_count + 1
    }
    else {
      cat("[",ticker,"] More than one record in file ",file,"\n")
      error_count = error_count + 1
    }
  }
  
  cat("[",ticker,"]", error_count, " error(s)\n")
  MarketData = MarketData[with(MarketData, order(Date)),]
  return(MarketData)
}
