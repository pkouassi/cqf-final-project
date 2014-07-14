#==============================================================================
# title           :monte_carlo_simulation_functions.R
# description     :Core function that price bonds, caps, swaptions
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

HeathJarrowMortonPricing = function(instrument,t,T_array,K_array,ForwardInputData,DiscountCurve,NumberSimulation=10000,GenType="rnorm") {
  # parallel computing set-up
  cl = makeCluster(detectCores())
  registerDoParallel(cl)
  cat("Number of core(s) to be used for calculation:",getDoParWorkers(),"\n")
  
  # set parameters for HJM framework
  NumberPC = 5
  MaxMaturity = 5
  maturityBucket = 1/12
  if (instrument == "bond") {
    NumberOfYear = max(T_array) #bond price computation requires projection of forward rates until T
  }
  else {
    # other products only require projection of forward rates until t
    NumberOfYear = t
  }
  
  timestep = 0.01 #size of timestep for projection
  NumberOfTimesteps = NumberOfYear/timestep
  
  MaturityList = seq(1/12,MaxMaturity,by=maturityBucket)
  drift = rep(NA,length(MaturityList))
  volatility_1 = rep(NA,length(MaturityList))
  volatility_2 = rep(NA,length(MaturityList))
  volatility_3 = rep(NA,length(MaturityList))
  volatility_4 = rep(NA,length(MaturityList))
  volatility_5 = rep(NA,length(MaturityList))
  
  
  for (j in seq(1,length(MaturityList))) {
    drift[j] = M(MaturityList[j])
    volatility_1[j] = PC1_volatility_fitted(MaturityList[j])
    volatility_2[j] = PC2_volatility_fitted(MaturityList[j])
    volatility_3[j] = PC3_volatility_fitted(MaturityList[j])
    volatility_4[j] = PC4_volatility_fitted(MaturityList[j])
    volatility_5[j] = PC5_volatility_fitted(MaturityList[j])
  }
  
  # create function to populate HJM matrix (performance reason)
  populate_row = function(i,mat,dX) {
    result = rep(NA,ncol(mat))
    #cat("row above:",mat[i-1,],"\n")
    #cat("j:",seq(1,ncol(mat)-1),"\n")
    for (j in seq(1,ncol(mat)-1)) 
    {
      # Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
      result[j] = mat[i-1,j] + drift[j]*timestep + 
        sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3],volatility_4[j]*dX[i-1,4],volatility_5[j]*dX[i-1,5])*sqrt(timestep) +
        ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
    }
    
    # Last row 
    # use backward difference for dF/dTau on last column
    result[ncol(mat)] = mat[i-1,j] + drift[j]*timestep + 
      sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3],volatility_4[j]*dX[i-1,4],volatility_5[j]*dX[i-1,5])*sqrt(timestep) +
      ((mat[i-1,j]-mat[i-1,j-1])/(
        maturityBucket))*timestep 
    
    return(result)
  }  
  populate_row.compiled = cmpfun(populate_row)
  
  # Pseudo random genetor or quasi random generator
  if (GenType == "rnorm") {
    dX_array = matrix(rnorm(NumberSimulation*NumberPC*NumberOfTimesteps, mean = 0, sd = 1),nrow=NumberSimulation,ncol=NumberPC*NumberOfTimesteps,byrow=FALSE)
  }
  else if (GenType == "sobol") {
    dX_array = sobol(NumberSimulation, dim = NumberPC*NumberOfTimesteps, normal = TRUE, scrambling = 3)
  }
  else if (GenType == "halton") {
    dX_array = halton(NumberSimulation, dim = NumberPC*NumberOfTimesteps, normal = TRUE, usetime = FALSE)
  }
  else if (GenType == "nag-sobol") {
    dX_array = quasirandom.nag(NumberSimulation,NumberPC*NumberOfTimesteps,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else if (GenType == "nag-niederreiter") {
    dX_array = quasirandom.nag(NumberSimulation,NumberPC*NumberOfTimesteps,"niederreiter","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else if (GenType == "nag-faure") {
    dX_array = quasirandom.nag(NumberSimulation,NumberPC*NumberOfTimesteps,"faure","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else {
    dX_array = matrix(rnorm(NumberSimulation*NumberPC*NumberOfTimesteps, mean = 0, sd = 1),nrow=NumberSimulation,ncol=NumberPC*NumberOfTimesteps,byrow=FALSE)
  }
  
  # Monte Carlo loop
  Result = foreach(k=1:NumberSimulation, .combine=rbind, .export=c("ComputeBondPrice","ComputeLIBORRates","ComputeCapPrice","ComputeCapletPrice","GetDiscountFactor","ComputeForwardStartingParSwapPrice","ComputePayerSwaptionPrice")) %dopar% {
    # for (k in seq(1,NumberSimulation)) {    
    if (k%%(NumberSimulation/20) == 0) cat((k/NumberSimulation)*100,"% ...\n")
    # matrix init
    mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
    mat[1,] = ForwardInputData[1:length(MaturityList)]
    # Take one new row of dX for this simulation
    dX = matrix(dX_array[k,],ncol=NumberPC,nrow=NumberOfTimesteps,byrow = TRUE)
    
    for (i in seq(2,nrow(mat))) {
      mat[i,] =  populate_row.compiled(i,mat,dX)
    }
    
    # Instrument specific code
    if (instrument == "bond") {
      # Code for bond
      bond_array = rep(NA,length(T_array)) 
      for (j in seq(1,length(T_array))) {
        bond_array[j] = ComputeBondPrice(mat,timestep,t,T_array[j])
      }
      res = c(bond=bond_array)
    }
    else if (instrument == "cap") {
      # Code for cap
      # We need the forward rates from t+0.25 to T
      libor_continuously_compounded_array = ComputeLIBORRates(mat,timestep,t,seq(t+0.25,max(T_array),by=0.25))
      libor_simply_compounded_array = 4*(exp(libor_continuously_compounded_array/4)-1) #3M compounding
      
      cap_array = rep(NA,length(K_array)*length(T_array))
      for (l in seq(1,length(T_array))) {
        for (j in seq(1,length(K_array))) {
          #cat((l-1)*length(K_array)+j,"T=",T_array[l],"K=",K_array[j],"\n")
          cap_array[(l-1)*length(K_array)+j] = ComputeCapPrice(mat,timestep,t,T_array[l],K_array[j],DiscountCurve)
        }
      }
      res = c(cap=cap_array,libor=libor_simply_compounded_array)
    }
    else if (instrument == "swap") {
      # Code for forward starting par swap
      swap_array = rep(NA,length(T_array))
      for (j in seq(1,length(T_array))) {
        swap_array[j] = ComputeForwardStartingParSwapPrice(mat,timestep,t,T_array[j],DiscountCurve)
      }      
      res=c(swap=swap_array)
    }
    else if (instrument == "swaption") {
      # Code for payer swaption. call on swap rate
      # calculate of forward swap rate
      swap_array = rep(NA,length(T_array))
      for (j in seq(1,length(T_array))) {
        swap_array[j] = ComputeForwardStartingParSwapPrice(mat,timestep,t,T_array[j],DiscountCurve)
      }
      
      swaption_array = rep(NA,length(K_array)*length(T_array))
      for (l in seq(1,length(T_array))) {
        for (j in seq(1,length(K_array))) {
          #cat((l-1)*length(K_array)+j,"T=",T_array[l],"K=",K_array[j],"\n")
          swaption_array[(l-1)*length(K_array)+j] = ComputePayerSwaptionPrice(mat,timestep,t,T_array[l],K_array[j],DiscountCurve)
        }
      }
      res=c(swaption=swaption_array,swap=swap_array)
    }  
  }
  stopCluster(cl)
  
  if (instrument == "bond") {
    # Result formating for bond
    price=rep(NA,length(T_array))
    simulation_array = matrix(NA,nrow=NumberSimulation,ncol=0)
    if (length(T_array) == 1) {
      cat("Bond Z[",t,",",T_array,"]=",price <- mean(Result[,"bond"]),"\n")
      simulation_array = cbind(simulation_array,Result[,"bond"])
    }
    else {
      for (j in seq(1,length(T_array))) {
        cat("Bond Z[",t,",",T_array[j],"]=",price[j] <- mean(Result[,paste("bond",j,sep="")]),"\n")
        simulation_array = cbind(simulation_array,Result[,paste("bond",j,sep="")])
      }
    }
    return(list(price=price,simulation=simulation_array))
  }
  else if (instrument == "cap") {
    # Result formating for cap
    # price computation
    price=matrix(NA,nrow=length(K_array),ncol=length(T_array))
    simulation_array = matrix(NA,nrow=NumberSimulation,ncol=0)
    if (length(K_array) == 1 && length(T_array) == 1) {
      cat("Cap[",t,",",T_array,",",K_array,"]=",price[1,1] <- mean(Result[,"cap"]),"\n")
      simulation_array = cbind(simulation_array,Result[,"cap"])
    }
    else {
      for (l in seq(1,length(T_array))) {
        for (j in seq(1,length(K_array))) {
          #cat("item",(l-1)*length(K_array)+j,"\n")
          cat("Cap[",t,",",T_array[l],",",K_array[j],"]=",price[j,l] <- mean(Result[,paste("cap",(l-1)*length(K_array)+j,sep="")]),"\n")
          simulation_array = cbind(simulation_array,Result[,paste("cap",j,sep="")])
        }
      }
    }
    
    # Implied volatility computation
    libor_T = seq(t+0.25,max(T_array),by=0.25)
    libor = rep(NA,length(libor_T))
    if (length(libor_T) == 1) {
      cat("Libor[",t,",",libor_T,"]=",libor <- mean(Result[,"libor"]),"\n")
    }
    else {
      for (j in seq(1,length(libor_T))) {
        cat("Libor[",t,",",libor_T[j],"]=",libor[j] <- mean(Result[,paste("libor",j,sep="")]),"\n")
      }
    }
    
    iv = matrix(NA,nrow=length(K_array),ncol=length(T_array))
    for (l in seq(1,length(T_array))) {
      #cat("T=",T_array[l],"\n")
      libor_list = libor[1:((T_array[l]-t)/0.25)]
      #print(libor_list)
      
      for (j in seq(1,length(K_array))) {
        iv[j,l] = Black76CapImpliedVolatility(t,T_array[l],K_array[j],libor_list,price[j,l])  
      }
    }
    
    return(list(price=price,iv=iv,libor=libor,simulation=simulation_array))
  }
  else if (instrument == "swap") {
    # Result formating for par swap
    # price computation
    price=rep(NA,length(T_array))
    simulation_array = matrix(NA,nrow=NumberSimulation,ncol=0)
    if (length(T_array) == 1) {
      cat("Forward Swap[",t,",",T_array,"]=",price <- mean(Result[,"swap"]),"\n")
      simulation_array = cbind(simulation_array,Result[,"swap"])
    }
    else {
      for (j in seq(1,length(T_array))) {
        cat("Forward Swap[",t,",",T_array[j],"]=",price[j] <- mean(Result[,paste("swap",j,sep="")]),"\n")
        simulation_array = cbind(simulation_array,Result[,paste("swap",j,sep="")])
      }
    }
    
    return(list(price=price,simulation=simulation_array))
  }
  else if (instrument == "swaption") {
    # Result formating for swaption
    # price computation
    price=matrix(NA,nrow=length(K_array),ncol=length(T_array))
    simulation_array = matrix(NA,nrow=NumberSimulation,ncol=0)
    if (length(K_array) == 1 && length(T_array) == 1) {
      cat("Swaption[",t,",",T_array,",",K_array,"]=",price[1,1] <- GetDiscountFactor(DiscountCurve,t)*mean(Result[,"swaption"]),"\n")
      simulation_array = cbind(simulation_array,Result[,"swaption"])
    }
    else {
      for (l in seq(1,length(T_array))) {
        for (j in seq(1,length(K_array))) {
          #cat("item",(l-1)*length(K_array)+j,"\n")
          cat("Swaption[",t,",",T_array[l],",",K_array[j],"]=",price[j,l] <- GetDiscountFactor(DiscountCurve,t)*mean(Result[,paste("swaption",(l-1)*length(K_array)+j,sep="")]),"\n")
          simulation_array = cbind(simulation_array,Result[,paste("swaption",j,sep="")])
        }
      }
    }
    
    # Implied volatility computation
    swap = rep(NA,length(T_array))
    if (length(T_array) == 1) {
      cat("Swap[",t,",",T_array,"]=",swap <- mean(Result[,"swap"]),"\n")
    }
    else {
      for (j in seq(1,length(T_array))) {
         cat("Swap[",t,",",T_array[j],"]=",swap[j] <- mean(Result[,paste("swap",j,sep="")]),"\n")
      }
    }
    
#     iv = matrix(NA,nrow=length(K_array),ncol=length(T_array))
#     for (l in seq(1,length(T_array))) {    
#       for (j in seq(1,length(K_array))) {
#         iv[j,l] = Black76SwaptionImpliedVolatility(t,T_array[l],K_array[j],swap[l],price[j,l])  
#       }
#     }
    
    return(list(price=price,swap=swap,simulation=simulation_array))
  }
  
}

# Calculate the dirft (without the the part df/dtau) 
M = function(Tau) {
   # This function carries out integration for all principal factors. 
   # It uses the fact that volatility is function of time in HJM model
   
   if (Tau == 0) {
     return(0)
   }
   else {
     dTau = 0.01
     NumberOfSlice = floor(Tau/dTau)
     
     #M1 / M2 / M3 / M4 / M5
     M1 = integrate(PC1_volatility_fitted_vector,0,Tau)$value
     M2 = integrate(PC2_volatility_fitted_vector,0,Tau)$value
     M3 = integrate(PC3_volatility_fitted_vector,0,Tau)$value
     M4 = integrate(PC4_volatility_fitted_vector,0,Tau)$value
     M5 = integrate(PC5_volatility_fitted_vector,0,Tau)$value
     
     return(PC1_volatility_fitted(Tau)*M1+PC2_volatility_fitted(Tau)*M2+PC3_volatility_fitted(Tau)*M3+PC4_volatility_fitted(Tau)*M4+PC5_volatility_fitted(Tau)*M5)
   }
}

# Compute Bond price
ComputeBondPrice = function(matrix,timestep,t, T) {
   # row 1 of the matrix is the valuation forward curve
   # row 2 is forward curve for valuation date + 1*timestep
   # row 3 is forward curve for valuation date + 2*timestep
   # first column of the matrix is the 1M forward which we use as proxy for r(t) (spot rate)
   # Compute the bond price which start at time t and matures at time T 
   # by integrating over the first column 
   t_index = t/timestep+1
   T_index = T/timestep+1  
   #cat("t_index:",t_index,"\n")
   #cat("T_index:",T_index,"\n")  
   return(exp(-1*sum((matrix[t_index:T_index,1])*timestep)))  
 }
 
# Compute Libor rates
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
   # libor formula: L = 1/tau * integral_0_tau(forward_curve)
   forward_curve_integration_func = function (x) (1/x)*integrate(forward_curve_func,0,x)$value
   
   return(sapply(T_array-t,forward_curve_integration_func))
}

# Compute cap price
ComputeCapPrice = function(matrix,timestep,t,T,K,DiscountCurve) {
  # A cap can be decomposed into quaterly caplets 
  # if t=0, the first caplet is skipped (no uncertainty)
  # i.e a 1Y cap that starts at t=0 can be decomposed into 3 caplets (0.25-0.5, 0.5-0.75, 0.75-1.0)
  
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
  #for 1*2 cap, we need 4 libor rates: [1,1.25], [1.25,1.50], [1.50,1.75], [1.75,2]
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
 
# Compute caplet price 
ComputeCapletPrice = function(t_start,t_end,K,libor,DiscountCurve) {
  #cat("libor=",libor,"/DF=",GetDiscountFactor(ValuationDateOISYieldCurve,t_end),"/Tau=",t_end-t_start,"\n")
  value = max(libor-K,0)*GetDiscountFactor(DiscountCurve,t_end)*(t_end-t_start)
  return(value)
} 
 
# Compute par swap rate for forward starting IRS
ComputeForwardStartingParSwapPrice = function(matrix,timestep,t,T,DiscountCurve) {
  # define cashflows. Fixed vs. Floating Swap with 3M frequency
  start_dates_array = seq(t,T-0.25,by=0.25)
  end_dates_array = seq(t+0.25,T,by=0.25)
  
  # calculate libor rates (continuously componded) for each libor_date
  # for 1*2 swap, we need 4 libor rates: [1,1.25], [1.25,1.50], [1.50,1.75], [1.75,2]
  libor_rates_cont_comp = ComputeLIBORRates(matrix,timestep,t,end_dates_array)
  libor_rates_quaterly_comp = 4*(exp(libor_rates_cont_comp/4)-1)
  
  tmpfunc = function(x) {
    fixed_leg = 0
    floating_leg = 0
    for (i in seq(1,length(end_dates_array))) {
      # Cashflows must be discounted to t (start date of the swap) not to today (0)
      # DF[0,Ti] = DF[0,t]*DF[t,Ti]
      # i.e. DF[t,Ti] = DF[0,Ti] / DF[0,t]
      fixed_leg = fixed_leg + 1*x*(end_dates_array[i]-start_dates_array[i])*(GetDiscountFactor(DiscountCurve,end_dates_array[i])/GetDiscountFactor(DiscountCurve,t))
      floating_leg = floating_leg + 1*libor_rates_quaterly_comp[i]*(end_dates_array[i]-start_dates_array[i])*(GetDiscountFactor(DiscountCurve,end_dates_array[i])/GetDiscountFactor(DiscountCurve,t))
    }
    #value = floating_leg - fixed_leg
    value = GetDiscountFactor(DiscountCurve,t) * (floating_leg - fixed_leg)
  }
  
  #value = uniroot(tmpfunc,lower=-0.5,upper=0.5)$root
  value = uniroot(tmpfunc,lower=-5,upper=5)$root
  #cat("par swap:",value,"\n")
  
  return(value)  
}  
 
# Compute payer swaption price
ComputePayerSwaptionPrice = function(matrix,timestep,t,T,K,DiscountCurve) {
  #Swaption pricing 
  #define cashflows
  start_dates_array = seq(t,T-0.25,by=0.25)
  end_dates_array = seq(t+0.25,T,by=0.25)
  
  #calculate libor rates (continuously componded) for each libor_date
  libor_rates_cont_comp = ComputeLIBORRates(matrix,timestep,t,end_dates_array)
  libor_rates_quaterly_comp = 4*(exp(libor_rates_cont_comp/4)-1)
  
  #brigo / mercurio 2006. p19
  value = 0
  for (i in seq(1,length(end_dates_array))) {
    value = value + (GetDiscountFactor(DiscountCurve,end_dates_array[i])/GetDiscountFactor(DiscountCurve,t))*(end_dates_array[i]-start_dates_array[i])*(libor_rates_quaterly_comp[i]-K)
  }
  
  value = max(value,0)
  #max(ComputeForwardStartingParSwapPrice(matrix,timestep,t,T,DiscountCurve)-K,0)*GetDiscountFactor(DiscountCurve,t)
  return(value)
}
 
 
