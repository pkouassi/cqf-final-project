#marginal default time
#we use the credit curve for one day

BMY_USD_XR=parseData("C://temp//markit","BMY","USD","XR")
DELL_USD_XR=parseData("C://temp//markit","DELLN","USD","XR")
HP_USD_XR=parseData("C://temp//markit","HPQ","USD","XR")
IBM_USD_XR=parseData("C://temp//markit","IBM","USD","XR")
PFE_USD_XR=parseData("C://temp//markit","PFE","USD","XR")

#Estimation of hazard rates (empirical marginal distribution)
BMY_USD_XR_MARGINAL = BMY_USD_XR[BMY_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & BMY_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
DELL_USD_XR_MARGINAL = DELL_USD_XR[DELL_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & DELL_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
HP_USD_XR_MARGINAL = HP_USD_XR[HP_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & HP_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
IBM_USD_XR_MARGINAL = IBM_USD_XR[IBM_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & IBM_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
PFE_USD_XR_MARGINAL = PFE_USD_XR[PFE_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & PFE_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]

#Credit Curve Bootstrapping
RR = 0.40
CDS1Y = new ("CreditDefaultSwap", maturity = 1, marketprice = BMY_USD_XR_MARGINAL$Spread1y*10000)
CDS2Y = new ("CreditDefaultSwap", maturity = 2, marketprice = BMY_USD_XR_MARGINAL$Spread2y*10000)
CDS3Y = new ("CreditDefaultSwap", maturity = 3, marketprice = BMY_USD_XR_MARGINAL$Spread3y*10000)
CDS4Y = new ("CreditDefaultSwap", maturity = 4, marketprice = BMY_USD_XR_MARGINAL$Spread4y*10000)
CDS5Y = new ("CreditDefaultSwap", maturity = 5, marketprice = BMY_USD_XR_MARGINAL$Spread5y*10000)
CDScol=c(CDS1Y,CDS2Y,CDS3Y,CDS4Y,CDS5Y)
YieldCurve = getYieldCurve(HistYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))
BMY_USD_XR_MARGINAL_CREDIT_CURVE = BootstrapCreditCurve(CDScol,RR,YieldCurve)

UTest = runif(5, min = 0, max = 1)

BMY_USD_XR_MARGINAL_CREDIT_CURVE@hazardrate

u = 1-exp(0.004)

hazardtime = function(arr) {
  result = rep(NA,length(arr))
  for (i in seq(1,length(arr))) {
    sum = 0
    for (j in seq(1,5)) {
      sum = sum + BMY_USD_XR_MARGINAL_CREDIT_CURVE@hazardrate[j] * 1
      if (abs(log(1-arr[i]))<=sum) {
        result[i] = paste(j-1,"-",j)
        break
      } 
    }  
  }
  return(result)  
}

hazardtime(runif(100, min = 0, max = 1))


sum = 0
for (i in seq(1,5)) {
  #cat("i=",i,"\n")
  sum = sum + BMY_USD_XR_MARGINAL_CREDIT_CURVE@hazardrate[i] * 1
  #cat("abs(log(1-u)):",abs(log(1-u))," // sum: ", sum,"\n")
  #cat("==>", (abs(log(1-u))<=sum),"\n")
  
  if (abs(log(1-u))<=sum) {
    return(paste(i-1,"-",i))
    break
  }
  
}


