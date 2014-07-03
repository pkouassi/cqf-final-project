#Compute Exact Defaut Time
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

#compute premium leg value
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
