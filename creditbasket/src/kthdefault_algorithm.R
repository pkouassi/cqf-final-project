
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
