ans_hjm1 = HeathJarrowMortonPricing("bond",0,c(1,2),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,100,"rnorm")

ans_hjm2 = HeathJarrowMortonPricing("cap",1,2,c(0.005,0.01,0.02,0.03,0.04),ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,100,"rnorm")

HeathJarrowMortonPricing = function(instrument,t,T_array,K_array,ForwardInputData,DiscountCurve,NumberSimulation=10000,GenType="rnorm") {
  #parallel computing set-up
  cl = makeCluster(detectCores())
  registerDoParallel(cl)
  cat("Number of core(s) to be used for calculation:",getDoParWorkers(),"\n")
  
  #set parameters for HJM framework
  MaxMaturity = 5
  maturityBucket = 1/12
  NumberOfYear = 2 #projection of the forward rates evolation over 2 years
  timestep = 0.01 #size of timestep for projection
  NumberOfTimesteps = NumberOfYear/timestep
  
  MaturityList = seq(1/12,MaxMaturity,by=maturityBucket) #1M rate is taken as proxy for Maturity=0
  drift = rep(NA,length(MaturityList))
  volatility_1 = rep(NA,length(MaturityList))
  volatility_2 = rep(NA,length(MaturityList))
  volatility_3 = rep(NA,length(MaturityList))
  
  for (j in seq(1,length(MaturityList))) {
    drift[j] = M(MaturityList[j])
    volatility_1[j] = PC1_volatility_fitted(MaturityList[j])
    volatility_2[j] = PC2_volatility_fitted(MaturityList[j])
    volatility_3[j] = PC3_volatility_fitted(MaturityList[j])
  }
  
  #create function to populate HJM matrix (performance reason)
  populate_row = function(i,mat,dX) {
    result = rep(NA,ncol(mat))
    #cat("row above:",mat[i-1,],"\n")
    #cat("j:",seq(1,ncol(mat)-1),"\n")
    for (j in seq(1,ncol(mat)-1)) 
    {
      #Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
      result[j] = mat[i-1,j] + drift[j]*timestep + 
        sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
        ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
    }
    
    #Last row 
    #use backward difference for dF/dTau on last column
    result[ncol(mat)] = mat[i-1,j] + drift[j]*timestep + 
      sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
      ((mat[i-1,j]-mat[i-1,j-1])/(
        maturityBucket))*timestep 
    
    return(result)
  }  
  populate_row.compiled = cmpfun(populate_row)
  
  # Pseudo random genetor or quasi random generator
  if (GenType == "rnorm") {
    dX_array = matrix(rnorm(NumberSimulation*3*NumberOfTimesteps, mean = 0, sd = 1),nrow=NumberSimulation,ncol=3*NumberOfTimesteps,byrow=FALSE)
  }
  else if (GenType == "sobol") {
    dX_array = rnorm.sobol(n = NumberSimulation, dimension = 3*NumberOfTimesteps , scrambling = 3)
  }
  else if (GenType == "nag-sobol") {
    dX_array = quasirandom.nag(NumberSimulation,3*NumberOfTimesteps,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else if (GenType == "nag-niederreiter") {
    dX_array = quasirandom.nag(NumberSimulation,3*NumberOfTimesteps,"niederreiter","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else if (GenType == "nag-faure") {
    dX_array = quasirandom.nag(NumberSimulation,3*NumberOfTimesteps,"faure","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else {
    dX_array = matrix(rnorm(NumberSimulation*3*NumberOfTimesteps, mean = 0, sd = 1),nrow=NumberSimulation,ncol=3*NumberOfTimesteps,byrow=FALSE)
  }
  
  # Monte Carlo loop
  Result = foreach(k=1:NumberSimulation, .combine=rbind, .export=c("ComputeBondPrice","ComputeLIBORRates","ComputeCapPrice","ComputeCapletPrice","GetDiscountFactor")) %dopar% {
  #for (k in seq(1,NumberSimulation)) {    
    if (k%%(NumberSimulation/20) == 0) cat((k/NumberSimulation)*100,"% ...\n")
    # matrix init
    mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
    mat[1,] = ForwardInputData[1:length(MaturityList)]
    # Take one new row of dX for this simulation
    dX = matrix(dX_array[k,],ncol=3,nrow=NumberOfTimesteps,byrow = TRUE)
    
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
      #libor_continuously_compounded_array = ComputeLIBORRates(mat,timestep,1,seq(1.25,5.00,by=0.25))
      libor_continuously_compounded_array = ComputeLIBORRates(mat,timestep,t,seq(t+0.25,max(T_array),by=0.25))
      libor_simply_compounded_array = 4*(exp(libor_continuously_compounded_array/4)-1) #3M compounding
      
      cap_array = matrix(NA,nrow=length(K_array),ncol=1)
      for (j in seq(1,length(K_array))) {
        cap_array[j] = ComputeCapPrice(mat,timestep,t,T_array,K_array[j],DiscountCurve)
      }
      res = c(cap=cap_array,libor=libor_simply_compounded_array)
    }
  }
  stopCluster(cl)
  
  if (instrument == "bond") {
    #Result formating for bond
    price=rep(NA,length(T_array))
    if (length(T_array) == 1) {
      cat("Bond Z[",t,",",T_array,"]=",price <- mean(Result[,"bond"]),"\n")
    }
    else {
      for (j in seq(1,length(T_array))) {
        cat("Bond Z[",t,",",T_array[j],"]=",price[j] <- mean(Result[,paste("bond",j,sep="")]),"\n")
      }
    }
    return(list(price=price))
  }
  else if (instrument == "cap") {
    #Result formating for cap
    price=rep(NA,length(K_array))
    if (length(K_array) == 1) {
      cat("Cap[",t,",",T_array,",",K_array,"]=",price <- mean(Result[,"cap"]),"\n")
    }
    else {
      for (j in seq(1,length(K_array))) {
        cat("Cap[",t,",",T_array,",",K_array[j],"]=",price[j] <- mean(Result[,paste("cap",j,sep="")]),"\n")
      }
    }
  }
  
}