#gather historical credit curve data
BMY_USD_XR=parseData("C://temp//markit","BMY","USD","XR")
DELL_USD_XR=parseData("C://temp//markit","DELLN","USD","XR")
HP_USD_XR=parseData("C://temp//markit","HPQ","USD","XR")
IBM_USD_XR=parseData("C://temp//markit","IBM","USD","XR")
PFE_USD_XR=parseData("C://temp//markit","PFE","USD","XR")

#only keep data from Monday 6-May-2013 to Friday 23-May-2014
BMY_USD_XR = BMY_USD_XR[BMY_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & BMY_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
DELL_USD_XR = DELL_USD_XR[DELL_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & DELL_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
HP_USD_XR = HP_USD_XR[HP_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & HP_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
IBM_USD_XR = IBM_USD_XR[IBM_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & IBM_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
PFE_USD_XR = PFE_USD_XR[PFE_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & PFE_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]

#gather historical yield curve

#bootstrap credit curve for 1y data

#IBM / first date

BootstrapHistoricCreditCurve = function(HistCDSData) {
  RecoveryRate = 0.40
  HistCreditCurve = as.data.frame(matrix(ncol=4, nrow=nrow(HistCDSData)))
  names(HistCreditCurve) = c("Date", "Ticker","SP_5Y", "DP_5Y")
  
  for (i in seq(1,nrow(HistCDSData)))
  {
    CDS1Y = new ("CreditDefaultSwap", maturity = 1, marketprice = HistCDSData[i,]$Spread1y*10000)
    CDS2Y = new ("CreditDefaultSwap", maturity = 2, marketprice = HistCDSData[i,]$Spread2y*10000)
    CDS3Y = new ("CreditDefaultSwap", maturity = 3, marketprice = HistCDSData[i,]$Spread3y*10000)
    CDS4Y = new ("CreditDefaultSwap", maturity = 4, marketprice = HistCDSData[i,]$Spread4y*10000)
    CDS5Y = new ("CreditDefaultSwap", maturity = 5, marketprice = HistCDSData[i,]$Spread5y*10000)
    CDScol=c(CDS1Y,CDS2Y,CDS3Y,CDS4Y,CDS5Y)
    YieldCurve = getYieldCurve(HistYieldCurveMatrix,HistCDSData[i,]$Date)
    tmp = BootstrapCreditCurve(CDScol,RecoveryRate,YieldCurve)
    
    HistCreditCurve$Date[i] = HistCDSData[i,]$Date
    HistCreditCurve$Ticker[i] = HistCDSData[i,]$Ticker
    HistCreditCurve$SP_5Y[i] = tmp@survivalprobability[5]
    HistCreditCurve$DP_5Y[i] = 1 - HistCreditCurve$SP_5Y[i]
  }
  
  return(HistCreditCurve)
}

BMY_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(BMY_USD_XR)
DELL_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(DELL_USD_XR)
HP_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(HP_USD_XR)
IBM_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(IBM_USD_XR)
PFE_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(PFE_USD_XR)

#calculate default probability difference
ComputeDifference = function(HistCreditCurve) {
  HistDifference = as.data.frame(matrix(ncol=3, nrow=(nrow(HistCreditCurve)-1)))
  names(HistDifference) = c("Date", "Ticker","DP_5Y_change")
  
  for (i in seq(2,nrow(HistCreditCurve))) {
    HistDifference$Date[i-1] = HistCreditCurve$Date[i]       
    HistDifference$Ticker[i-1] = HistCreditCurve$Ticker[i]
    HistDifference$DP_5Y_change[i-1] = (HistCreditCurve$DP_5Y[i] - HistCreditCurve$DP_5Y[i-1])    
  }
  return(HistDifference)
}

BMY_USD_XR_PDdiff = ComputeDifference(BMY_USD_XR_HistCreditCurve)
DELL_USD_XR_PDdiff = ComputeDifference(DELL_USD_XR_HistCreditCurve)
HP_USD_XR_PDdiff = ComputeDifference(HP_USD_XR_HistCreditCurve)
IBM_USD_XR_PDdiff = ComputeDifference(IBM_USD_XR_HistCreditCurve)
PFE_USD_XR_PDdiff = ComputeDifference(PFE_USD_XR_HistCreditCurve)

#represent plot
plot(BMY_USD_XR_PDdiff$Date,BMY_USD_XR_PDdiff$DP_5Y_change,type="l")

BMY_USD_XR_PDdiff$DP_5Y_change[32]

#represent histograms
hist(IBM_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(PFE_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(HP_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(BMY_USD_XR_PDdiff$DP_5Y_change, breaks=30)

#correlation for gaussian copula
transform_pddiff_to_normal = function(X) {
  fn = ecdf(X) 
  U = fn(X)
  Z = qnorm(U)
  return(Z)
}

#must clean data earlier
Z_BMY_USD_XR = transform_pddiff_to_normal(BMY_USD_XR_PDdiff$DP_5Y_change)
Z_PFE_USD_XR = transform_pddiff_to_normal(PFE_USD_XR_PDdiff$DP_5Y_change)
Z_IBM_USD_XR = transform_pddiff_to_normal(IBM_USD_XR_PDdiff$DP_5Y_change)
Z_DELL_USD_XR = transform_pddiff_to_normal(DELL_USD_XR_PDdiff$DP_5Y_change)
Z_HP_USD_XR = transform_pddiff_to_normal(HP_USD_XR_PDdiff$DP_5Y_change)

correlation_exclude_spikes = function(Z1,Z2) {
  #check for Inf
  spikes_for_Z1 = !(Z1 != Inf)
  spikes_for_Z2 = !(Z2 != Inf)
  spikes_for_both = spikes_for_Z1 | spikes_for_Z2  
  cat(sum(spikes_for_both == TRUE)," date(s) excluded in both time series...\n")
  
  return(cor(Z1[spikes_for_both==FALSE],Z2[spikes_for_both==FALSE]))
}

myPDdiffMatrix = rbind(Z_BMY_USD_XR,Z_PFE_USD_XR,Z_IBM_USD_XR,Z_DELL_USD_XR,Z_HP_USD_XR)

#Correlation for Gaussian copula
DefaultProbabilityMatrix = matrix(NA, 
                           nrow=nrow(myPDdiffMatrix),
                           ncol=nrow(myPDdiffMatrix),
                           byrow = TRUE);

for (i in seq(1,nrow(myPDdiffMatrix))) {
  for (j in seq(1,nrow(myPDdiffMatrix))) {
    DefaultProbabilityMatrix[i,j] = correlation_exclude_spikes(myPDdiffMatrix[i,],myPDdiffMatrix[j,])
  }
}

#linear measure (Pearson)
#cor(x,y,method = "pearson")

#Correlation for Student t copula
#cor(x,y,method = "kendall")
#rho = sin(0.5*Pi*rho_k)




