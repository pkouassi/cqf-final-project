#Credit Curve Bootstrapping

BootstrapCreditCurve = function (CDSCollection,RecoveryRate,YieldCurve) {
  #Credit Curve initialization
  CreditCurve = new ("CreditCurve", time = rep(NA,length(CDSCollection)), survivalprobability = rep(NA,length(CDSCollection)), hazardrate=rep(NA,length(CDSCollection)))
  
  for (i in seq(1,length(CDSCollection))) {
    CreditCurve@time[i] = CDSCollection[[i]]@maturity
    CreditCurve@spread[i] = CDSCollection[[i]]@marketprice
  }
  
  #first maturity pillar   
  CreditCurve@survivalprobability[1] = (1-RecoveryRate)/((1-RecoveryRate)+CreditCurve@time[1]*(CreditCurve@spread[1]/10000))
  CreditCurve@hazardrate[1] = (-1*log(CreditCurve@survivalprobability[1]))/CreditCurve@time[1]
  
  #subsequent maturity pillars
  if (length(CDSCollection)>1) {
    for (i in seq(2,length(CDSCollection))) {
      last_term = GetSurvivalProbability(CreditCurve,CreditCurve@time[i-1]) * (1-RecoveryRate) / (1-RecoveryRate + (CreditCurve@time[i]-CreditCurve@time[i-1]) * (CreditCurve@spread[i]/10000))
      
      #assumption is that we have a CDS for each year
      #loop through every CDS until the year before the maturity of current CDS (ith CDS)
      first_term = 0
      for (j in seq(1,i-1)) {
        if (j==1) {
          #specific case for t=0. survival probability of t=0 is 1
          term = GetDiscountFactor(YieldCurve,CreditCurve@time[j]) * ((1-RecoveryRate) * 1 - (1-RecoveryRate + (CreditCurve@time[j]-0) * (CreditCurve@spread[i]/10000)) * GetSurvivalProbability(CreditCurve,CreditCurve@time[j]))
        }
        else {
          term = GetDiscountFactor(YieldCurve,CreditCurve@time[j]) * ((1-RecoveryRate) * GetSurvivalProbability(CreditCurve,CreditCurve@time[j-1]) - (1-RecoveryRate + (CreditCurve@time[j]-CreditCurve@time[j-1]) * (CreditCurve@spread[i]/10000)) * GetSurvivalProbability(CreditCurve,CreditCurve@time[j]))
        }
        first_term = first_term + term
      }
      
      quotient = (GetDiscountFactor(YieldCurve,CreditCurve@time[i]) * (1-RecoveryRate + (CreditCurve@time[i]-CreditCurve@time[i-1]) * (CreditCurve@spread[i]/10000)))
      first_term = first_term / quotient

      #assign survival probability and hazard rate
      CreditCurve@survivalprobability[i] = first_term + last_term  
      CreditCurve@hazardrate[i] = (-1/1)*log(CreditCurve@survivalprobability[i]/CreditCurve@survivalprobability[i-1])

    }
  }
  
  #DisplayCreditCurve = as.data.frame(matrix(ncol=5, nrow=length(CDSCollection)))
  #names(DisplayCreditCurve) = c("Time", "MarketSpread","DF", "SurvivalProbability","HazardRate")
  #for (i in seq(1,length(CDSCollection)) ) {
  #  DisplayCreditCurve$Time[i] = CreditCurve@time[i] 
  #  DisplayCreditCurve$MarketSpread[i] = CreditCurve@spread[i] 
  #  DisplayCreditCurve$DF[i] = GetDiscountFactor(YieldCurve,CreditCurve@time[i]) 
  #  DisplayCreditCurve$SurvivalProbability[i] = CreditCurve@survivalprobability[i] 
  #  DisplayCreditCurve$HazardRate[i] = CreditCurve@hazardrate[i] 
  #}  
  #print(DisplayCreditCurve)
  
  return(CreditCurve)
}


#gather historical yield curve
#bootstrap credit curve for 1y data
#IBM / first date

BootstrapHistoricCreditCurve = function(HistCDSData,HistYieldCurve) {
  RecoveryRate = 0.40
  HistCreditCurve = matrix(list(),nrow(HistCDSData),3)
  colnames(HistCreditCurve) = c("Date", "Ticker","CreditCurve")
  
  for (i in seq(1,nrow(HistCDSData)))
  {
    CDS1Y = new ("CreditDefaultSwap", maturity = 1, marketprice = HistCDSData[i,]$Spread1y*10000)
    CDS2Y = new ("CreditDefaultSwap", maturity = 2, marketprice = HistCDSData[i,]$Spread2y*10000)
    CDS3Y = new ("CreditDefaultSwap", maturity = 3, marketprice = HistCDSData[i,]$Spread3y*10000)
    CDS4Y = new ("CreditDefaultSwap", maturity = 4, marketprice = HistCDSData[i,]$Spread4y*10000)
    CDS5Y = new ("CreditDefaultSwap", maturity = 5, marketprice = HistCDSData[i,]$Spread5y*10000)
    CDScol=c(CDS1Y,CDS2Y,CDS3Y,CDS4Y,CDS5Y)
    YieldCurve = getYieldCurve(HistYieldCurve,HistCDSData[i,]$Date)
    tmp = BootstrapCreditCurve(CDScol,RecoveryRate,YieldCurve)
    
    HistCreditCurve[[i,"Date"]] = HistCDSData[i,]$Date
    HistCreditCurve[[i,"Ticker"]] = HistCDSData[i,]$Ticker
    HistCreditCurve[[i,"CreditCurve"]] = tmp
  }
  
  return(HistCreditCurve)
}

ConvertHistoricCreditCurveToDataframe = function(HistCreditCurve) {
  #Extract specific info from the historical credit curve and store them in a dataframe
  HistCreditCurveDataframe = as.data.frame(matrix(NA,ncol=4, nrow=nrow(HistCreditCurve)))
  names(HistCreditCurveDataframe) = c("Date", "Ticker","SP_5Y", "DP_5Y")
  
  for (i in seq(1,nrow(HistCreditCurve))) {
    HistCreditCurveDataframe$Date[i] = HistCreditCurve[i,"Date"]
    HistCreditCurveDataframe$Ticker[i] = HistCreditCurve[i,"Ticker"]
    HistCreditCurveDataframe$SP_5Y[i] = HistCreditCurve[[i,"CreditCurve"]]@survivalprobability[5] #5 years SP
    HistCreditCurveDataframe$DP_5Y[i] = 1 - HistCreditCurve[[i,"CreditCurve"]]@survivalprobability[5] #5 years DP  
  }
  
  return(HistCreditCurveDataframe)  
}