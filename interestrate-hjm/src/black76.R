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

#implement root finding using newton raphason
Black76ImpliedVolatilityBisection = function(type,F,K,Time,Premium, SigmaMin, SigmaMax, Iteration, MaxIteration, MaxError) {
  
  SigmaMid = SigmaMin + (SigmaMax - SigmaMin)/2;
  SigmaMinPremium = Black76OptionPricing(type,F,K,Time,SigmaMin)
  SigmaMaxPremium = Black76OptionPricing(type,F,K,Time,SigmaMax)
  SigmaMidPremium = Black76OptionPricing(type,F,K,Time,SigmaMid)
  
  if (Iteration > MaxIteration) 
  {
    cat(sprintf("Max Iteration has been reached. Root not found\n"))
    return(NA);
  }
  
  if (abs(SigmaMidPremium-Premium) < MaxError) 
  {
    cat(sprintf("Root found after %i iterations\n", Iteration))
    return(SigmaMid);     
  }
  else if (SigmaMidPremium<Premium) {
    return(Black76ImpliedVolatilityBisection(type,F,K,Time,Premium,SigmaMid,SigmaMax,Iteration+1,MaxIteration,MaxError));
  }
  else if (SigmaMidPremium>Premium) {
    return(Black76ImpliedVolatilityBisection(type,F,K,Time,Premium,SigmaMin,SigmaMid,Iteration+1,MaxIteration,MaxError));
  }  
}

BSImpliedVolatilityNewtonRaphson = function(type,S,X,Time,r,Premium, Sigma, Iteration, MaxIteration, MaxError) {
  
  SigmaPremium = BSOptionPricing(type,S,X,Time,r,Sigma)
  
  if (Iteration > MaxIteration) 
  {
    cat(sprintf("Max Iteration has been reached. Root not found\n"))
    return(NA);
  }
  
  if (abs(SigmaPremium-Premium) < MaxError) 
  {
    cat(sprintf("Root found after %i iterations\n", Iteration))
    return(Sigma);     
  }
  else {
    # search sigma so that f(sigma) = 0
    # f(sigma) = OPtionPrice(sigma) - Premium 
    # f'(sigma) = dOPtionPrice(sigma)/dsigma = vega (Vega of the option using sigma as volatility)
    Vega = BSOptionGreeks(type,S,X,Time,r,Sigma,"vega")
    # x_k+1 = x_k - f(x_k) / f'(x_k)
    # sigma_k+1 = sigma_k - OPtionPrice(sigma_k) / Vega(sigma_k)
    NextSigma = Sigma - ((SigmaPremium-Premium)/Vega)
    return(BSImpliedVolatilityNewtonRaphson(type,S,X,Time,r,Premium, NextSigma, Iteration+1, MaxIteration, MaxError))
  }
  
}

#Black76 implied to be coded
#install.packages("fOptions")
#require(fOptions)
#attributes(Black76Option(TypeFlag = "c", FT = 100, X = 100, Time = 0.5, r = 0.10, sigma = 0.25))
#Black76Option(TypeFlag = "c", FT = 100, X = 100, Time = 0.5, r = 0.10, sigma = 0.25)$price

# Black 76 pricing formula
# to be verified
Black76OptionGreeks = function(type,S,X,Time,r,sigma,greek) {
  d1 = (log(S/X)+(r+0.5*sigma^2)*Time)/(sigma*sqrt(Time))
  d2 = (log(S/X)+(r-0.5*sigma^2)*Time)/(sigma*sqrt(Time))
  
  if (greek == "vega") {
    return (S*sqrt(Time)*(1/sqrt(2*pi))*exp(-(d1^2)/2))
  }
  else if (greek == "delta") {
    if (type == "call") {
      return (pnorm(d1))
    }
    else {
      return (pnorm(d1)-1)
    }       
  }
  else if (greek == "gamma") {
    return (((1/sqrt(2*pi))*exp(-(d1^2)/2))/(S*sigma*sqrt(Time)))
  }  
}


Black76CapPricing = function(t,T,K,libor_rates_array,sigma) {
  #define cashflows
  if (t == 0) {
    start_dates_array = seq(0.25,T-0.25,by=0.25)
    end_dates_array = seq(0.5,T,by=0.25)
  }
  else
  {
    #these are the start of period for each caplet; date at which libor is observed
    #settlement is done at the end of the period
    start_dates_array = seq(t,T-0.25,by=0.25)
    end_dates_array = seq(t+0.25,T,by=0.25)
  }
  
  #verifiy that we have one libor rate for each period
  if (length(start_dates_array) != length(libor_rates_array)) {
    cat("error: there is not the same the number of periods and libor rates\n")
    cat("period start dates:",start_dates_array,"\n")
    cat("period end dates:",start_dates_array,"\n")
    cat("libor rates:",libor_rates_array,"\n")
    return()
  }
  
  value = 0
  for (i in seq(1,length(start_dates_array))) {
    value = value + Black76OptionPricing("call",libor_rates_array[i],K,start_dates_array[i],sigma)*GetDiscountFactor(ValuationDateOISYieldCurve,end_dates_array[i])*(end_dates_array[i]-start_dates_array[i])/(1+libor_rates_array[i]*(end_dates_array[i]-start_dates_array[i]))
  }
  
  return(value)
}

Black76CapImpliedVolatility = function(t,T,K,libor_rates_array,premium) {
  #define objective function
  f = function(sigma) return(Black76CapPricing(t,T,K,libor_rates_array,sigma)-premium)
  return(uniroot(f,lower=0,upper=10)$root)
}