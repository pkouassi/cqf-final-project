parseData = function(pathname,ticker,ccy,docclause) {
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


BMY_USD_XR=parseData("D://temp//data","BMY","USD","XR")
DELL_USD_XR=parseData("D://temp//data","DELLN","USD","XR")
HP_USD_XR=parseData("D://temp//data","HPQ","USD","XR")
IBM_USD_XR=parseData("D://temp//data","IBM","USD","XR")
PFE_USD_XR=parseData("D://temp//data","PFE","USD","XR")

write.table(BMY_USD_XR, file = "D://temp//data/BMY_USD_XR.csv", sep = ",", qmethod = "double",row.names = FALSE)
write.table(DELL_USD_XR, file = "D://temp//data/DELL_USD_XR.csv", sep = ",", qmethod = "double",row.names = FALSE)
write.table(HP_USD_XR, file = "D://temp//data/HP_USD_XR.csv", sep = ",", qmethod = "double",row.names = FALSE)
write.table(IBM_USD_XR, file = "D://temp//data/IBM_USD_XR.csv", sep = ",", qmethod = "double",row.names = FALSE)
write.table(PFE_USD_XR, file = "D://temp//data/PFE_USD_XR.csv", sep = ",", qmethod = "double",row.names = FALSE)

zz=read.csv("D://temp//data/BMY_USD_XR.csv", header = TRUE)

par(mfrow=c(3,2))
plot(BMY_USD_XR$Date,BMY_USD_XR$Spread5y,type="l")
plot(DELL_USD_XR$Date,DELL_USD_XR$Spread5y,type="l")
plot(HP_USD_XR$Date,HP_USD_XR$Spread5y,type="l")
plot(IBM_USD_XR$Date,IBM_USD_XR$Spread5y,type="l")
plot(PFE_USD_XR$Date,PFE_USD_XR$Spread5y,type="l")

