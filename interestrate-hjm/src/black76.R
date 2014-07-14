#==============================================================================
# title           :black76.R
# description     :Black76 formula, Caps/Floors and Swaptions using Black76
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# Black 76 pricing formula
Black76OptionPricing = function(type,F,K,Time,sigma) {
  d1 = (log(F/K)+(0.5*sigma^2)*Time)/(sigma*sqrt(Time))
  d2 = (log(F/K)-(0.5*sigma^2)*Time)/(sigma*sqrt(Time))
  
  #REVIEW - NO r in black formula
  if (type == "call") {
    return (F*pnorm(d1)-K*pnorm(d2))
  }
  else {
    return (K*pnorm(-d2)-F*pnorm(-d1))
  }
}

# Cap pricing using Black76
Black76CapPricing = function(t,T,K,libor_rates_array,sigma) {
  # define cashflows
  if (t == 0) {
    start_dates_array = seq(0.25,T-0.25,by=0.25)
    end_dates_array = seq(0.5,T,by=0.25)
  }
  else
  {
    # these are the start of period for each caplet; date at which libor is observed
    # settlement is done at the end of the period
    start_dates_array = seq(t,T-0.25,by=0.25)
    end_dates_array = seq(t+0.25,T,by=0.25)
  }
  
  # verifiy that we have one libor rate for each period
  if (length(start_dates_array) != length(libor_rates_array)) {
    cat("error: there is not the same the number of periods and libor rates\n")
    cat("period start dates:",start_dates_array,"\n")
    cat("period end dates:",end_dates_array,"\n")
    cat("libor rates:",libor_rates_array,"\n")
    return()
  }
  
  value = 0
  for (i in seq(1,length(start_dates_array))) {
    caplet = Black76OptionPricing("call",libor_rates_array[i],K,start_dates_array[i],sigma)*GetDiscountFactor(ValuationDateOISYieldCurve,end_dates_array[i])*(end_dates_array[i]-start_dates_array[i])/(1+libor_rates_array[i]*(end_dates_array[i]-start_dates_array[i]))
    #cat("caplet:",caplet,"\n")
    value = value + caplet
  }
  #cat("cap:",value,"\n")
  
  return(value)
}

# Cap Implied volatility calculation (root finding)
Black76CapImpliedVolatility = function(t,T,K,libor_rates_array,premium) {
  # define objective function
  f = function(sigma) return(Black76CapPricing(t,T,K,libor_rates_array,sigma)-premium)

  res <- try( uniroot(f,lower=0,upper=10), silent=TRUE )
  if (inherits(res, "try-error")) 
  { 
    warning("unable to find a root\n")
    return(NA)
  }
  else 
  { 
    return(res$root)
  }
}

# Swaption pricing using Black76
Black76SwaptionPricing = function(t,T,K,F,sigma) {
  # we assume quaterly payments for the swaptions
  # m=4
  start_dates_array = seq(t,T-0.25,by=0.25)
  end_dates_array = seq(t+0.25,T,by=0.25)
  
  # Formula from Brigo / Mercurio 2006. p20.
  value = 0
  for (i in seq(1,length(end_dates_array))) {
    value = value + (end_dates_array[i]-start_dates_array[i])*GetDiscountFactor(ValuationDateOISYieldCurve,end_dates_array[i])
  }
  value = value * Black76OptionPricing("call",F,K,t,sigma)  

  return(value)
}

# Swaption Implied volatility calculation (root finding)
Black76SwaptionImpliedVolatility = function(t,T,K,F,premium) {
  # define objective function
  f = function(sigma) return(Black76SwaptionPricing(t,T,K,F,sigma)-premium)
  
  res <- try( uniroot(f,lower=0,upper=10), silent=TRUE )
  if (inherits(res, "try-error")) 
  { 
    warning("unable to find a root\n")
    return(NA)
  }
  else 
  { 
    return(res$root)
  }  
}
