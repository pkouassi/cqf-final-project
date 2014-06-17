#Credit Curve Bootstrapping

#class definition
setClass("CreditDefaultSwap", 
         representation(
           maturity = "numeric", 
           marketprice = "numeric" #market spread
         )
)

setClass("YieldCurve",
         representation(
           time = "vector",
           discountfactor = "vector")
)

setClass("CreditCurve",
         representation(
           time = "vector",
           spread = "vector",
           survivalprobability = "vector",
           hazardrate = "vector")
)

#function definition
GetSurvivalProbability = function(CreditCurve,t) {
  result = NA
  t_index = match(t,CreditCurve@time)
  if (!is.na(CreditCurve@survivalprobability[t_index])) {
    result = CreditCurve@survivalprobability[t_index]
  }
  return (result)
}

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

