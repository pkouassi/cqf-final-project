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