#==============================================================================
# title           :market_data_functions.R
# description     :Defines function that load his cds spreads and yield curve
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# Function that parse market data files (Markit) to fetch Historical CDS spreads 
parseHistoricalCreditData = function(pathname,ticker,ccy,docclause,start_date,end_date,frequency="daily") {
  files = list.files(path=pathname, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  files_within_date_range = rep(NA,length(files))
  
  #date filtering
  #daily frequency
  if (frequency == "daily") {
    for (i in seq(1,length(files))) {
      file_date = as.Date(substr(basename(files[i]), 6, 12),"%d%b%y") #file e.g. C://temp//markit/Data-01Apr11.csv
      if (file_date>=start_date && file_date<=end_date) {
        files_within_date_range[i] = files[i]
      }
    }    
  }
  #weekly frequency. take friday data
  else if (frequency == "weekly") {
    for (i in seq(1,length(files))) {
      file_date = as.Date(substr(basename(files[i]), 6, 12),"%d%b%y") #file e.g. C://temp//markit/Data-01Apr11.csv
      if (file_date>=start_date && file_date<=end_date && weekdays(file_date)=="Friday") {
        files_within_date_range[i] = files[i]
      }
    }    
  }
  else {
    #if not daily or weekly. assume daily
    for (i in seq(1,length(files))) {
      file_date = as.Date(substr(basename(files[i]), 6, 12),"%d%b%y") #file e.g. C://temp//markit/Data-01Apr11.csv
      if (file_date>=start_date && file_date<=end_date) {
        files_within_date_range[i] = files[i]
      }
    }   
  }
  files_within_date_range = files_within_date_range[!is.na(files_within_date_range)]

  MarketData = as.data.frame(matrix(ncol=11, nrow=length(files_within_date_range)))
  names(MarketData) = c("Date", "Ticker","ShortName", "Tier","DocClause","Spread1y","Spread2y","Spread3y","Spread4y","Spread5y","AvRating")
  
  date_format = "%d-%b-%y"  
  error_count=0
  i=1
  for (file in files_within_date_range) {
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
  
  cat("Historical CDS Spreads for [",ticker,"] loaded.", error_count, " error(s)\n")
  MarketData = MarketData[with(MarketData, order(Date)),]
  return(MarketData)
}

# Function that parse market data files (Bloomberg) to fetch Historical Yield Curve
# We have only weekly data (fridays)
parseHistoricalYieldCurve = function(filename) {  
  historic_yielcurve=read.csv(paste(getwd(),filename,sep=""), header = TRUE,stringsAsFactors = FALSE)
  
  HistoricYieldCurveMatrix = matrix(list(),nrow=length(unique(historic_yielcurve$Date)),ncol=2,byrow = TRUE);
  colnames(HistoricYieldCurveMatrix) = c("Date", "YieldCurve")
  
  date_format = "%Y-%m-%d" #format: 2014-04-23
  #BBG Ticker definition for USD Yield Curve
  USD_1D = "S0023D 1D BLC2"
  USD_2D = "S0023D 2D BLC2"
  USD_1W = "S0023D 1W BLC2"
  USD_1M = "S0023D 1M BLC2"
  USD_2M = "S0023D 2M BLC2"
  USD_3M = "S0023D 3M BLC2"
  USD_6M = "S0023D 6M BLC2"
  USD_1Y = "S0023D 1Y BLC2"
  USD_2Y = "S0023D 2Y BLC2"
  USD_3Y = "S0023D 3Y BLC2"
  USD_4Y = "S0023D 4Y BLC2"
  USD_5Y = "S0023D 5Y BLC2"
  USD_6Y = "S0023D 6Y BLC2"
  
  k=1  
  for (date in unique(historic_yielcurve$Date)) {
    yielcurve = historic_yielcurve[(historic_yielcurve$Date == date),]
    #cat("date number ",k,"\n")  
    HistoricYieldCurveMatrix[[k,"Date"]] = as.Date(date,date_format)
    HistoricYieldCurveMatrix[[k,"YieldCurve"]] = new ("YieldCurve", time = rep(NA,nrow(yielcurve)), discountfactor = rep(NA,nrow(yielcurve)))  
    
    for (i in seq(1,nrow(yielcurve))) {
      
      if (yielcurve$Id[i] == USD_1D) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 1/365
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])          
      }
      else if (yielcurve$Id[i] == USD_2D) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 2/365
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_1W) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 7/365
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_1M) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 1/12
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_2M) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 2/12
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_3M) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 3/12
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_6M) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 6/12
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_1Y) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 1
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_2Y) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 2
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_3Y) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 3
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_4Y) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 4
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_5Y) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 5
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }
      else if (yielcurve$Id[i] == USD_6Y) {
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@time[i] = 6
        HistoricYieldCurveMatrix[[k,"YieldCurve"]]@discountfactor[i] = as.numeric(yielcurve$DiscountFactor[i])
      }  
    }  
    
    k = k+1
  }
  
  #sort by date
  HistoricYieldCurveMatrix = HistoricYieldCurveMatrix[order(as.numeric(HistoricYieldCurveMatrix[,"Date"])),]  
  cat("USD Yield Curves loaded from",min(historic_yielcurve$Date),"to", max(historic_yielcurve$Date),"\n")
  return(HistoricYieldCurveMatrix)
}

#export/import data
export_dataframe = function(dataframe,filename) {
  write.table(dataframe, file = paste(getwd(),"/../data/",filename,sep=""), sep = ",", col.names = NA,qmethod = "double")
}

import_dataframe = function(filename) {
  cat("Importing file",filename,"\n")
  return(read.table(paste(getwd(),"/../data/",filename,sep=""), header = TRUE, sep = ",", row.names = 1))
}

#retrieve yield curve as of paste date (previous friday yieldcurve)
getYieldCurve = function(historic_yieldcurve,date) {
  result = NA
  #cat("requested date=",date,"\n")
  for (i in seq(1,nrow(historic_yieldcurve))) {
    if (i == nrow(historic_yieldcurve)) {
      if (date == historic_yieldcurve[[i,1]]) {
        result = historic_yieldcurve[[i,2]]
        break
      }
    }
    else {
      if (date>=historic_yieldcurve[[i,1]] && date<historic_yieldcurve[[i+1,1]]) 
      {
        result = historic_yieldcurve[[i,2]]
        break
      }
    }
  }
  return(result)     
}

#retrieve survival probability for a specific date
GetSurvivalProbability = function(CreditCurve,t) {
  result = NA
  t_index = match(t,CreditCurve@time)
  if (!is.na(CreditCurve@survivalprobability[t_index])) {
    result = CreditCurve@survivalprobability[t_index]
  }
  return (result)
}

#retrieve discount factor for a specific date
GetDiscountFactor = function(YieldCurve,t) {
  min_time = min(YieldCurve@time)
  min_time_index = which.min(YieldCurve@time)
  max_time = max(YieldCurve@time)
  max_time_index = which.max(YieldCurve@time)
  
  result = NA
  
  if (length(t)==1) {
    if (t < 0) {
      cat("Warning: t is negative. discountfactor not calculated for this case")
    }
    else if (t == 0) {
      result = 1 #df of t=0 is 1
    }
    else if (t>0 && t<min_time) {
      #df of t=0 is 1
      result = 1 + (YieldCurve@discountfactor[min_time_index]-1)*(t/min_time)
    }
    else if (t >= max_time) {
      result = YieldCurve@discountfactor[max_time_index]
    }
    else {
      #i.e t falls between 2 maturity for which we have the discount factor
      for (i in seq(1,length(YieldCurve@time)-1)) {
        if (t>= YieldCurve@time[i] && t<YieldCurve@time[i+1]) {
          result = YieldCurve@discountfactor[i] + (YieldCurve@discountfactor[i+1]-YieldCurve@discountfactor[i])*((t-YieldCurve@time[i])/(YieldCurve@time[i+1]-YieldCurve@time[i]))
        }                                                                                                                 
      }
    }  
  }
  return (result)  
}

#Vectorized version of GetDiscountFactor
GetDiscountFactorVector = function(YieldCurve,t_array){
  return(sapply(t_array,GetDiscountFactor,YieldCurve=YieldCurve))
}
