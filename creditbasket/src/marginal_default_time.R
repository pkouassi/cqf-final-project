#marginal default time
#we use the credit curve for one day

BMY_USD_XR=parseHistoricCreditData("C://temp//markit","BMY","USD","XR")
DELL_USD_XR=parseHistoricCreditData("C://temp//markit","DELLN","USD","XR")
HP_USD_XR=parseHistoricCreditData("C://temp//markit","HPQ","USD","XR")
IBM_USD_XR=parseHistoricCreditData("C://temp//markit","IBM","USD","XR")
PFE_USD_XR=parseHistoricCreditData("C://temp//markit","PFE","USD","XR")

#Estimation of hazard rates (empirical marginal distribution)
BMY_USD_XR_MARGINAL = BMY_USD_XR[BMY_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & BMY_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
DELL_USD_XR_MARGINAL = DELL_USD_XR[DELL_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & DELL_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
HP_USD_XR_MARGINAL = HP_USD_XR[HP_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & HP_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
IBM_USD_XR_MARGINAL = IBM_USD_XR[IBM_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & IBM_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
PFE_USD_XR_MARGINAL = PFE_USD_XR[PFE_USD_XR$Date>=as.Date("23-MAY-2014","%d-%b-%Y") & PFE_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]

#Credit Curve Bootstrapping
#RR = 0.40
#CDS1Y = new ("CreditDefaultSwap", maturity = 1, marketprice = BMY_USD_XR_MARGINAL$Spread1y*10000)
#CDS2Y = new ("CreditDefaultSwap", maturity = 2, marketprice = BMY_USD_XR_MARGINAL$Spread2y*10000)
#CDS3Y = new ("CreditDefaultSwap", maturity = 3, marketprice = BMY_USD_XR_MARGINAL$Spread3y*10000)
#CDS4Y = new ("CreditDefaultSwap", maturity = 4, marketprice = BMY_USD_XR_MARGINAL$Spread4y*10000)
#CDS5Y = new ("CreditDefaultSwap", maturity = 5, marketprice = BMY_USD_XR_MARGINAL$Spread5y*10000)
#CDScol=c(CDS1Y,CDS2Y,CDS3Y,CDS4Y,CDS5Y)
#YieldCurve = getYieldCurve(HistYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))
#BMY_USD_XR_MARGINAL_CREDIT_CURVE = BootstrapCreditCurve(CDScol,RR,YieldCurve)

#UTest = runif(5, min = 0, max = 1)
#
#BMY_USD_XR_MARGINAL_CREDIT_CURVE@hazardrate
#ttt = BMY_USD_XR_MARGINAL_CREDIT_CURVE
#BootstrapHistoricCreditCurve(BMY_USD_XR_MARGINAL)[1,"CreditCurve"]



HazardExactDefaultTime = function(CreditCurve,u_array) {
  #determine the year of default
  #by comparing abs(log(1-u_array[i])) and sum (sum of lambda_i*delta_ti)
  exacttimeofdefault = rep(Inf,length(u_array))
  for (i in seq(1,length(u_array))) {
    #debug_string = rep(NA,length(CreditCurve@hazardrate))
    sum = 0
    for (j in seq(1,length(CreditCurve@hazardrate))) {
      sum = sum + CreditCurve@hazardrate[j] * 1
      #debug_string[j] = paste("i=",i,"j=",j,"abs(log(1-u))=",abs(log(1-u_array[i])),"sum=",sum,"flag=",(abs(log(1-u_array[i]))<=sum))
      if (abs(log(1-u_array[i]))<=sum) {
        year = j-1
        
        if (j == 1) {
          #survival probability of t=0 is 1
          deltat = (-1/CreditCurve@hazardrate[j])*log(1-u_array[i])
        }
        else {
          deltat = (-1/CreditCurve@hazardrate[j])*log((1-u_array[i])/CreditCurve@survivalprobability[j-1])  
        }
        
        exacttimeofdefault[i] = year + deltat
        
        #for (k in seq(1,j)) {
        #  cat(debug_string[k],"\n")
        #}
        break
      } 
    }  
  }
  return(exacttimeofdefault)
}



#HazardExactDefaultTime(BMY_USD_XR_MARGINAL_CREDIT_CURVE,aaa)

#set.seed(24)
#aaa = runif(300000, min = 0, max = 1)
#HazardExactDefaultTime(BMY_USD_XR_MARGINAL_CREDIT_CURVE,aaa)

#sapply(aaa,HazardExactDefaultTime,CreditCurve=BMY_USD_XR_MARGINAL_CREDIT_CURVE)


