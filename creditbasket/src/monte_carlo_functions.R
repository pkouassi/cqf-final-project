#==============================================================================
# title           :monte_carlo_functions.R
# description     :defines functions which will be used during Monte Carlo sim
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# Compute Exact Defaut Time
ConvertToDefaultTime = function(CreditCurve,u_array) {
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

# Compute premium leg value
ComputePremiumLeg = function(YieldCurve,NumberCDS,k,tau_arr) {
  #calculation is discretised per annum
  Maturity = 5
  tau_arr = tau_arr[tau_arr != Inf]
  tau_arr = sort(tau_arr)
  cashflowdates = sort(c(seq(0,Maturity+1),tau_arr))
  #print(cashflowdates)
  fraction = NumberCDS/NumberCDS
  result = 0
  
  if (is.na(tau_arr[k])) {
    #cat("No",k,"th default available\n")
    #cat("Only ",length(tau_arr), "available\n")
    
    for (l in seq(1,length(tau_arr)+1)) {
      fraction = (NumberCDS-l+1)/NumberCDS
      #cat("**** fraction: ",NumberCDS-l+1,"/ NumberCDS ****\n")
      
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
      else if (l==length(tau_arr)+1) {
        while(cashflowdates[i]<=Maturity) {          
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
      fraction = (NumberCDS-l+1)/NumberCDS
      #cat("**** fraction: ",NumberCDS-l+1,"/ NumberCDS ****\n")
      
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

UniformCorrelationMatrix = function(rho,n) matrix(rho,nrow=n,ncol=n) + (1-rho)*diag(n)

GetFlatCreditCurve = function(x,yc) {
  return(BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 2, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 3, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 4, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 5, marketprice = x)),0.40,yc))
}
