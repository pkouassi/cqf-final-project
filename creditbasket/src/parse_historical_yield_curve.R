parseHistoricYieldCurve = function(filename) {  
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
  cat("YieldCurves loaded from",min(historic_yielcurve$Date),"to", max(historic_yielcurve$Date),"\n")
  return(HistoricYieldCurveMatrix)
}


#getYieldCurve(HistoricYieldCurveMatrix,as.Date("12-OCT-2013","%d-%b-%Y"))

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

HistoricYieldCurveMatrix = parseHistoricYieldCurve("/../data/HistoricYieldCurveWeekly1Y-2013-2014.csv")

# plot_x_lim = c(1,5);
# plot_y_lim = c(0.9,1);
# for (i in seq(1,nrow(HistoricYieldCurveMatrix))) {
#   data = HistoricYieldCurveMatrix[[i,2]]
#   par(new=TRUE);
#   plot(data@time,data@discountfactor,type="l",xlim=plot_x_lim, ylim=plot_y_lim)
# }
