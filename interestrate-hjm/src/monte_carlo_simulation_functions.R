#Trapezium rule / integration
 integral_trapezium_rule = function(f,x0,xn,N) {
  dx = (xn-x0)/N
  
  #0.5 * dx * ((y0 + yn) + 2 (y1 + y2 + ... + yn-1))
  result = 0
  
  #yo / yn
  #cat("y0:",f(x0),"\n")
  #cat("yn:",f(xn),"\n")
  result = result + 0.5 * f(x0) + 0.5 * f(xn)
  
  #y1 ... yn-1
  for (i in seq(1,N-1)) {
    #cat("y(",x0 + i*dx,"):",f(x0 + i*dx),"\n")
    result = result + f(x0 + i*dx)  
  }
  
  #multiply by dx
  result = result * dx
  
  return(result)
}

#PC1_volatility_fitted = function(Tau) {
#   return(0.0064306548)
# }
 
# PC2_volatility_fitted = function(Tau) {
#   return(-0.0035565431 + Tau*(-0.0005683999) + Tau^2 * 0.0001181915 + Tau^3 * (-0.0000035939))
# }
 
# PC3_volatility_fitted = function(Tau) {
#   return(-0.0047506715 + Tau * 0.0017541783 + Tau ^ 2 * (-0.0001415249) + Tau ^ 3 * 0.0000031274)
# }
 
M = function(Tau) {
   # This funciton carries out integration for all principal factors. 
   # It uses the fact that volatility is function of time in HJM model
   
   if (Tau == 0) {
     return(0)
   }
   else {
     dTau = 0.01
     NumberOfSlice = floor(Tau/dTau)
     
     #M1 / M2 / M3
     #M1 = integral_trapezium_rule(PC1_volatility_fitted,0,Tau,NumberOfSlice)
     #M2 = integral_trapezium_rule(PC2_volatility_fitted,0,Tau,NumberOfSlice)
     #M3 = integral_trapezium_rule(PC3_volatility_fitted,0,Tau,NumberOfSlice)
     M1 = integrate(PC1_volatility_fitted_vector,0,Tau)$value
     M2 = integrate(PC2_volatility_fitted_vector,0,Tau)$value
     M3 = integrate(PC3_volatility_fitted_vector,0,Tau)$value
     
     return(PC1_volatility_fitted(Tau)*M1+PC2_volatility_fitted(Tau)*M2+PC3_volatility_fitted(Tau)*M3)
   }
}
 
ComputeBondPrice = function(matrix,timestep,t, T) {
   #row 1 of the matrix is the valuation forward curve
   #row 2 is forward curve for valuation date + 1*timestep
   #row 3 is forward curve for valuation date + 2*timestep
   #first column of the matrix is the 1M forward which we use as proxy for r(t) (spot rate)
   #Compute the bond price which start at time t and matures at time T 
   #by integrating over the first column 
   t_index = t/timestep+1
   T_index = T/timestep+1  
   #cat("t_index:",t_index,"\n")
   #cat("T_index:",T_index,"\n")  
   return(exp(-1*sum((matrix[t_index:T_index,1])*timestep)))  
 }
 
ComputeLIBORRates = function(matrix,timestep,t,T_array) {
   t_index = t/timestep+1
   #cat("t_index:",t_index,"\n")
   result = rep(NA,ncol(matrix))
   
   x1 = seq(1/12,by=1/12,length=ncol(matrix))
   x2 = x1^2
   x3 = x2^3
   y = matrix[t_index,]
   fit = lm(y~x1+x2+x3)
   
   b0 = as.numeric(fit$coefficients["(Intercept)"])
   b1 = as.numeric(fit$coefficients["x1"])
   b2 = as.numeric(fit$coefficients["x2"])
   b3 = as.numeric(fit$coefficients["x3"])
   forward_curve_func = function(x) return(b0+b1*x+b2*x^2+b3*x^3)
   forward_curve_integration_func = function (x) integrate(forward_curve_func,0,x)$value
   
   return(sapply(T_array-t,forward_curve_integration_func))
}

ComputeCapPrice = function(matrix,timestep,t,T,K,DiscountCurve) {
  #A cap can be decomposed into quaterly caplets 
  #if t=0, the first caplet is skipped (no uncertainty)
  #i.e a 1Y cap that starts at t=0 can be decomposed into 3 caplets (0.25-0.5, 0.5-0.75, 0.75-1.0)
  
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
  
  #calculate libor rates (continuously componded) for each libor_date
  #for 1*1 cap, we need 4 libor rates: 1, 1.25, 1.50, 1.75
  libor_rates_cont_comp = ComputeLIBORRates(matrix,timestep,t,end_dates_array)
  libor_rates_quaterly_comp = 4*(exp(libor_rates_cont_comp/4)-1)
  
  #print(libor_dates)
  #cat("libor cont comp:",libor_rates_cont_comp,"\n")
  #cat("libor 3m com:",libor_rates_quaterly_comp,"\n")
  
  value = 0
  for (i in seq(1,length(start_dates_array))) {
    caplet = ComputeCapletPrice(start_dates_array[i],end_dates_array[i],K,libor_rates_quaterly_comp[i],DiscountCurve)
    value = value + caplet
    #cat("caplet:",caplet,"\n")
  }
  #cat("cap:",value,"\n")
  return(value)
}
 

 
ComputeCapletPrice = function(t_start,t_end,K,libor,DiscountCurve) {
  #cat("libor=",libor,"/DF=",GetDiscountFactor(ValuationDateOISYieldCurve,t_end),"/Tau=",t_end-t_start,"\n")
  value = max(libor-K,0)*GetDiscountFactor(DiscountCurve,t_end)*(t_end-t_start)
  return(value)
} 
 
 
ComputeSwaptionPrice = function(matrix,timestep,t,T,K) {
  #Swaption pricing. Fixed vs. Floating Swap with 3M frequency
  if (t == 0) {
    warning("t should be greater than zero\n")
  }
  else
  {
    start_dates_array = seq(t,T-0.25,by=0.25)
    end_dates_array = seq(t+0.25,T,by=0.25)
  }
  
  #calculate libor rates (continuously componded) for each libor_date
  libor_rates_cont_comp = ComputeLIBORRates(matrix,timestep,t,libor_dates_array)
  libor_rates_quaterly_comp = 4*(exp(libor_rates_cont_comp/4)-1)
  
  spread = 0
}
