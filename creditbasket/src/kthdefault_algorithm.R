
DF = GetDiscountFactorVector(YieldCurve,seq(1,5))
tau_list = c(0.8,1.45,3.6,Inf,Inf)

premium_leg_part = rep(NA,5)
l=1
for (j in seq(1,5)) {
  if (tau_list[l]>=j && tau_list[l]<(j+1)) {
    
  }
}


yeti = function(YieldCurve,t1,t2) {
  result = 0
  
  if (ceiling(t1) != ceiling(t2)) {
    #t1 and t2 are not part of same year    
    cat("t1 and t2 not in the same year\n")
    result = result + GetDiscountFactor(YieldCurve,t1)*(ceiling(t1)-t1)
    cat("DF(0,",t1,")*",(ceiling(t1)-t1)," -- added:",GetDiscountFactor(YieldCurve,t1)*(ceiling(t1)-t1),"\n")

    year_after_t1 = if (ceiling(t1)==t1) t1+1 else ceiling(t1)
    year_before_t2 = floor(t2) 
    for (i in seq(year_after_t1+1,year_before_t2)) {
      result = result + GetDiscountFactor(YieldCurve,i)*1
      cat("DF(0,",i,")*DeltaT -- added:",GetDiscountFactor(YieldCurve,i)*1,"\n")
    }
    result = result + GetDiscountFactor(YieldCurve,t2)*(t2-floor(t2))
    cat("added:",GetDiscountFactor(YieldCurve,t2)*(t2-floor(t2)),"\n")
  }
  else {
    cat("t1 and t2 in the same year\n")
    result = result + GetDiscountFactor(YieldCurve,t2)*(t2-t1)
  }
  
  return(result)
}

compute_premium_leg = function(YieldCurve,k,tau_arr) {
  #calculation is discretised per annum
  last_year = 5
  number_asset = 5
  tau_arr = tau_arr[tau_arr != Inf]
  tau_arr = sort(tau_arr)
  cashflowdates = sort(c(seq(0,last_year+1),tau_arr))
  #print(cashflowdates)
  fraction = number_asset/number_asset
  result = 0
  
  if (is.na(tau_arr[k])) {
    #cat("No",k,"th default available\n")
    #cat("Only ",length(tau_arr), "available\n")
    
    for (l in seq(1,length(tau_arr))) {
      fraction = (number_asset-l+1)/5
      #cat("**** fraction: ",number_asset-l+1,"/ 5 ****\n")
      
      if (l==1) {
        #start from 2nd cashflow date (first one is zero)
        i=2
        while(cashflowdates[i]<=tau_arr[l]) {
          #cat("l=",l,"==> cashflowdate:",cashflowdates[i],"\n")
          value = fraction*GetDiscountFactor(YieldCurve,cashflowdates[i])*(cashflowdates[i]-cashflowdates[i-1])
          result = result + value
          #cat("value=",value,"|result=",result,"\n")
          i = i+1
        }
      }
      else if (l==length(tau_arr)) {
        #last perdiod until 5Y (because no kth default)
        while(cashflowdates[i]<=last_year) {
          #cat("l=",l,"==> cashflowdate:",cashflowdates[i],"\n")
          value = fraction*GetDiscountFactor(YieldCurve,cashflowdates[i])*(cashflowdates[i]-cashflowdates[i-1])
          result = result + value
          #cat("value=",value,"|result=",result,"\n")
          i = i+1
        }
      }
      else {
        #start from cashflow date after previous tau_arr element
        i = match(tau_arr[l-1],cashflowdates) + 1
        while(cashflowdates[i]<=tau_arr[l]) {
          #cat("l=",l,"==> cashflowdate:",cashflowdates[i],"\n")
          value = fraction*GetDiscountFactor(YieldCurve,cashflowdates[i])*(cashflowdates[i]-cashflowdates[i-1])
          result = result + value
          #cat("value=",value,"|result=",result,"\n")
          i = i+1
        }
      }
    }
  }
  else {
    #cat(k,"th default is available\n")
    for (l in seq(1,k)) {
      fraction = (number_asset-l+1)/5
      #cat("**** fraction: ",number_asset-l+1,"/ 5 ****\n")
      
      if (l==1) {
        #start from 2nd cashflow date (first one is zero)
        i=2
        while(cashflowdates[i]<=tau_arr[l]) {
          #cat("l=",l,"==> cashflowdate:",cashflowdates[i],"\n")
          value = fraction*GetDiscountFactor(YieldCurve,cashflowdates[i])*(cashflowdates[i]-cashflowdates[i-1])
          result = result + value
          #cat("value=",value,"|result=",result,"\n")
          i = i+1
        }
      }
      else {
        #start from cashflow date after previous tau_arr element
        i = match(tau_arr[l-1],cashflowdates) + 1
        while(cashflowdates[i]<=tau_arr[l]) {
          #cat("l=",l,"==> cashflowdate:",cashflowdates[i],"\n")
          value = fraction*GetDiscountFactor(YieldCurve,cashflowdates[i])*(cashflowdates[i]-cashflowdates[i-1])
          result = result + value
          #cat("value=",value,"|result=",result,"\n")
          i = i+1
        }
      }
    }
  }
  return(result)
}
