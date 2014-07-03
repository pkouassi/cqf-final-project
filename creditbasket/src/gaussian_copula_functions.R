FTDS_GaussianCopula = function(CreditCurveCollection,DiscountCurve,CorrelationMatrix,RecoveryRate,NumberSimulation) {
  NumberCDS = length(CreditCurveCollection)
  #Test if correlation matrix is positive definite
  A_gaussian = t(chol(CorrelationMatrix))
  
  ZMatrix_gaussian = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
  #ZMatrix_gaussian = matrix(rnorm(NumberSimulation*NumberCDS, mean = 0, sd = 1),ncol=NumberCDS,nrow=NumberSimulation,byrow=FALSE)
  
  XMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  #we impose correlation
  for (i in seq(1,NumberSimulation)) {
    XMatrix_gaussian[i,] = t(A_gaussian %*% ZMatrix_gaussian[i,]) # t() in order to keep X as a row vector
  }
  
  #Use normal CDF to map to uniform vector U
  UMatrix_gaussian = pnorm(XMatrix_gaussian)
  
  TauMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  for (i in seq(1,NumberCDS)) {
    TauMatrix_gaussian[,i] = HazardExactDefaultTime(CreditCurveCollection[[i]],UMatrix_gaussian[,i])
  }
  
  FTDS_LegCalculation_gaussian = matrix(data = NA,ncol=4, nrow=NumberSimulation)
  colnames(FTDS_LegCalculation_gaussian) = c("DefaultLeg","PremiumLeg","tau_1","spread")
  
  #first to default basket CDS
  for (i in seq(1,NumberSimulation)) {
    if ((i/NumberSimulation*100)%%25 == 0) cat((i/NumberSimulation)*100,"% ...\n")
    
    tau_list = sort(TauMatrix_gaussian[i,])
    tau_1 = tau_list[1]
    
    if (tau_1 < 0.25) tau_1 = 0.25
    
    #if (tau_1 < 0.25) {
    #  if (tau_list[2] > 0.25) {
    #    tau_1 = tau_list[2]
    #  }
    #  else {
    #    if (tau_list[3] > 0.25) {
    #      tau_1 = tau_list[3]
    #    }
    #    else {
    #      tau_1 = tau_list[4]
    #    }
    #  }
    #}
    
    
    #default leg calculation // in continuous time
    default_leg = 0
    if (tau_1 == Inf) {
      #i.e. tau_1 > 5. Number of default is below k during the life of the contract
      default_leg = 0
    }
    else {
      default_leg = (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,tau_1) * (1/NumberCDS)
      #df = integrate(GetDiscountFactorVector,lower=0,upper=tau_1,YieldCurve=DiscountCurve)$value
      #df = GetDiscountFactor(DiscountCurve,tau_1) 
      #default_leg = (1-RecoveryRate)* df * (1/NumberCDS)
      #cat("1-R:",1-RecoveryRate," / tau_1:",tau_1," / df:",df,"1/NumberCDS:",1/NumberCDS,"default leg:",default_leg,"\n")
      
      #for (j in seq(1,5)) {
      #  if (tau_1>(j-1) && tau_1<=j) {
      #    default_leg = default_leg + (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,j) * (1/NumberCDS)
      #  }
      #}
    }
    
    #premium leg calculation // in disctrete time (yearly payment)
    #One coding solution is to create a variable that accumulates PL at each dt = 0.01 and will need a fiited discounting curve for this increment.
    #integrate GetDiscountFactor from 0 to min_tau
    premium_leg = 0
    if (tau_1 == Inf) {
      #i.e. No default within the life of the contract
      for (j in seq(1,5)) {
        premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,j) * 1
      }
      #premium_leg = integrate(GetDiscountFactorVector,lower=0,upper=5,YieldCurve=DiscountCurve)$value
      #premium_leg = premium_leg*(5/5)
    }
    #else if (tau_1 <0.25) {
    #  premium_leg = (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,0.25) * 0.25 * (NumberCDS/NumberCDS)
    #}
    else {
      j=1
      while (j<tau_1 & j<5) {
        premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,j) * 1
        j = j+1
      }
      premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,tau_1) * (tau_1-(j-1))
      #premium_leg = integrate(GetDiscountFactorVector,lower=0,upper=tau_1,YieldCurve=DiscountCurve)$value
      
      #for (j in seq(1,5)) {
      #  if (tau_1>j) {
      #    premium_leg = premium_leg + (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,j) * (NumberCDS/NumberCDS)
      #  }
      #  else {
      #    break     
      #  }
      #}
      #premium_leg = premium_leg + (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,j) * (tau_1-(j-1)) * (NumberCDS/NumberCDS)
    }
    
    #don't take into account when Tau is too small (i.e <3M)
    #if (tau_1 > 0.25) {
      FTDS_LegCalculation_gaussian[i,"DefaultLeg"] = default_leg
      FTDS_LegCalculation_gaussian[i,"PremiumLeg"] = premium_leg  
      FTDS_LegCalculation_gaussian[i,"tau_1"] = tau_1 
      FTDS_LegCalculation_gaussian[i,"spread"] = default_leg/premium_leg
    #}
    
  }
  
  
  SimulationResult = cbind(TauMatrix_gaussian,FTDS_LegCalculation_gaussian[,"DefaultLeg"],FTDS_LegCalculation_gaussian[,"PremiumLeg"],FTDS_LegCalculation_gaussian[,"tau_1"],FTDS_LegCalculation_gaussian[,"spread"])
  
  expectation_ftds_default_leg_gaussian = mean(FTDS_LegCalculation_gaussian[,"DefaultLeg"])
  expectation_ftds_premium_leg_gaussian = mean(FTDS_LegCalculation_gaussian[,"PremiumLeg"])
  #expectation_ftds_default_leg_gaussian = mean(FTDS_LegCalculation_gaussian[!is.na(FTDS_LegCalculation_gaussian[,"DefaultLeg"]),])
  #expectation_ftds_premium_leg_gaussian = mean(FTDS_LegCalculation_gaussian[!is.na(FTDS_LegCalculation_gaussian[,"PremiumLeg"]),])
  #expectation_ftds_spread_gaussian = expectation_ftds_default_leg_gaussian/expectation_ftds_premium_leg_gaussian
  expectation_ftds_spread_gaussian = mean(FTDS_LegCalculation_gaussian[,"spread"])
  
  for (i in seq(1,NumberCDS)) {
    cat("Number of Default for CDS",i,":",sum(TauMatrix_gaussian[,i]!=Inf),"(percentage:",sum(TauMatrix_gaussian[,i]!=Inf)/NumberSimulation*100,"%)","\n")
  }
  
  counter_morethanonedefault = 0
  counter_totaldefault = 0
  for (i in seq(1,NumberSimulation)) {
    if (sum(TauMatrix_gaussian[i,]!=Inf)>1) counter_morethanonedefault = counter_morethanonedefault + 1
    if (sum(TauMatrix_gaussian[i,]!=Inf)>0) counter_totaldefault = counter_totaldefault + 1
  }
  cat("Number of simulation where more than 1 default:", counter_morethanonedefault, "\n")
  cat("Number of simulation with default:", counter_totaldefault, "\n")
  
  cat("======> expectation default leg:",expectation_ftds_default_leg_gaussian,"\n")
  cat("======> expectation premium leg:",expectation_ftds_premium_leg_gaussian,"\n")
  
  #convergence diagram
  step=10
  nbobservation = round(NumberSimulation/step)
  expectation_spread_array = rep(NA,nbobservation)
  
  for (i in seq(1,nbobservation)) {
    #expectation_default_leg_array[i] = mean(LegCalculation_gaussian[1:i,"DefaultLeg"])
    #expectation_premium_leg_array[i] = mean(LegCalculation_gaussian[1:i,"PremiumLeg"])  
    expectation_spread_array[i] = mean(FTDS_LegCalculation_gaussian[1:i*step,"spread"])
    #expectation_spread_array[i] = expectation_default_leg_array[i]/expectation_premium_leg_array[i] 
  }
  plot(seq(1,nbobservation),expectation_spread_array, type="l", log="x")
  
  return(list(result=expectation_ftds_spread_gaussian,result2=expectation_ftds_default_leg_gaussian/expectation_ftds_premium_leg_gaussian,matsim=SimulationResult))
}

BasketCDSPricing_GaussianCopulaV2 = function(CreditCurve1,CreditCurve2,CreditCurve3,CreditCurve4,CreditCurve5,DiscountCurve,CorrelationMatrix,RecoveryRate,k=1,NumberSimulation=300000) {
  NumberCDS = 5
  #Test if correlation matrix is positive definite
  A_gaussian = t(chol(CorrelationMatrix)) # CorrelationMatrix = A_gaussian %*% t(A_gaussian)
  
  ZMatrix_gaussian = matrix(rnorm(NumberSimulation*NumberCDS, mean = 0, sd = 1),ncol=NumberCDS,nrow=NumberSimulation,byrow=FALSE)
  #using sobol numbers
  #ZMatrix_gaussian = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
  #ZMatrix_gaussian = quasirandom.nag(NumberSimulation,NumberCDS,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  
  XMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  UMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  TauMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  
  #we impose correlation
  for (i in seq(1,NumberSimulation)) {
    XMatrix_gaussian[i,] = t(A_gaussian %*% ZMatrix_gaussian[i,]) # t() in order to keep X as a row vector
  }
  
  UMatrix_gaussian = pnorm(XMatrix_gaussian)
  
  for (i in seq(1,NumberCDS)) {
    CC = NULL
    if (i == 1) {
      CC = CreditCurve1
    }
    else if (i == 2) {
      CC = CreditCurve2
    }
    else if (i == 3) {
      CC = CreditCurve3
    }
    else if (i == 4) {
      CC = CreditCurve4
    }
    else if (i == 5) {
      CC = CreditCurve5
    }
    TauMatrix_gaussian[,i] = HazardExactDefaultTime(CC,UMatrix_gaussian[,i])
  }
  
  LegCalculation_gaussian = matrix(data = NA,ncol=3, nrow=NumberSimulation)
  colnames(LegCalculation_gaussian) = c("DefaultLeg","PremiumLeg","Spread")
  #colnames(LegCalculation_gaussian) = c("DefaultLeg","PremiumLeg","DefaultLegCont","PremiumLegCont")
  
  #kth to default basket CDS
  for (i in seq(1,NumberSimulation)) {
    if ((i/NumberSimulation*100)%%25 == 0) cat((i/NumberSimulation)*100,"% ...\n")
    tau_list = sort(TauMatrix_gaussian[i,])
    tau_k = tau_list[k]
    
    if (tau_k<0.5) tau_k = 0.5
    
    #default leg calculation // in continuous time
    default_leg = 0
    if (tau_k == Inf) {
      #i.e. tau_k > 5. Number of default is below k during the life of the contract
      default_leg = 0
    }
    else {
      default_leg = (1-RecoveryRate)*GetDiscountFactor(DiscountCurve,tau_k) * (1/5)
    }
    
    #premium leg calculation // in disctrete time (yearly payment)
    #One coding solution is to create a variable that accumulates PL at each dt = 0.01 and will need a fiited discounting curve for this increment.
    #integrate GetDiscountFactor from 0 to min_tau
    premium_leg = 0
    if (tau_list[1] == Inf) {
      #i.e. No default within the life of the contract
      for (j in seq(1,5)) {
        premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,j)*1
      }
      premium_leg = premium_leg*(5/5)
    }
    else {
      premium_leg = compute_premium_leg(DiscountCurve,k,tau_list)
    }
    
    LegCalculation_gaussian[i,"DefaultLeg"] = default_leg
    LegCalculation_gaussian[i,"PremiumLeg"] = premium_leg
    LegCalculation_gaussian[i,"Spread"] = default_leg/premium_leg
    
    #test in continuous tinme for k=1
    #LegCalculation_gaussian[i,"DefaultLegCont"] = (1-R)*GetDiscountFactor(DiscountCurve,tau_list[1])*(1/5)
    #LegCalculation_gaussian[i,"PremiumLegCont"] = GetDiscountFactor(DiscountCurve,tau_list[1])*(5/5)
  }
  
  Simulation = cbind(TauMatrix_gaussian[,1],TauMatrix_gaussian[,2],TauMatrix_gaussian[,3],TauMatrix_gaussian[,4],TauMatrix_gaussian[,5],LegCalculation_gaussian[,"DefaultLeg"],LegCalculation_gaussian[,"PremiumLeg"],LegCalculation_gaussian[,"Spread"])
  
  expectation_default_leg_gaussian= mean(LegCalculation_gaussian[,"DefaultLeg"])
  expectation_premium_leg_gaussian= mean(LegCalculation_gaussian[,"PremiumLeg"])
  spread_gaussian_1 = expectation_default_leg_gaussian/expectation_premium_leg_gaussian
  spread_gaussian_2 = mean(LegCalculation_gaussian[,"Spread"])
  
  cat("======> expectation default leg:",mean(LegCalculation_gaussian[,"DefaultLeg"]),"\n")
  cat("======> expectation premium leg:",mean(LegCalculation_gaussian[,"PremiumLeg"]),"\n")
  #cat("======> expectation default leg (cont. time):",mean(LegCalculation_gaussian[,"DefaultLegCont"]),"\n")
  #cat("======> expectation premium leg (cont. time):",mean(LegCalculation_gaussian[,"PremiumLegCont"]),"\n")
  
  #convergence diagram
  nbobservation = round(NumberSimulation/10)
  expectation_default_leg_array = rep(NA,nbobservation)
  expectation_premium_leg_array = rep(NA,nbobservation)
  expectation_spread_1_array = rep(NA,nbobservation)
  expectation_spread_2_array = rep(NA,nbobservation)
  
  for (i in seq(1,nbobservation)) {
    expectation_default_leg_array[i] = mean(LegCalculation_gaussian[1:i*10,"DefaultLeg"])
    expectation_premium_leg_array[i] = mean(LegCalculation_gaussian[1:i*10,"PremiumLeg"])
    expectation_spread_1_array[i] = expectation_default_leg_array[i]/expectation_premium_leg_array[i] 
    expectation_spread_2_array[i] = mean(LegCalculation_gaussian[1:i*10,"Spread"])
  }
  matplot(seq(1,nbobservation),cbind(expectation_spread_1_array,expectation_spread_2_array), type="l", log="x")
  
  return(list(result1=spread_gaussian_1*10000,result2=spread_gaussian_2*10000,matsim=Simulation)) #result is return in basis point
}

BasketCDSPricing_GaussianCopula = function(CreditCurve1,CreditCurve2,CreditCurve3,CreditCurve4,CreditCurve5,DiscountCurve,CorrelationMatrix,RecoveryRate,k=1,NumberSimulation=300000) {
  NumberCDS = 5
  #Test if correlation matrix is positive definite
  A_gaussian = t(chol(CorrelationMatrix)) # CorrelationMatrix = A_gaussian %*% t(A_gaussian)
  
  ZMatrix_gaussian = cbind(rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1))
  #using sobol numbers
  #ZMatrix_gaussian = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
  #ZMatrix_gaussian = quasirandom.nag(NumberSimulation,NumberCDS,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  
  XMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  UMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  TauMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  
  #we impose correlation
  for (i in seq(1,NumberSimulation)) {
    XMatrix_gaussian[i,] = t(A_gaussian %*% ZMatrix_gaussian[i,]) # t() in order to keep X as a row vector
  }
  
  UMatrix_gaussian = pnorm(XMatrix_gaussian)
  
  for (i in seq(1,NumberCDS)) {
    CC = NULL
    if (i == 1) {
      CC = CreditCurve1
    }
    else if (i == 2) {
      CC = CreditCurve2
    }
    else if (i == 3) {
      CC = CreditCurve3
    }
    else if (i == 4) {
      CC = CreditCurve4
    }
    else if (i == 5) {
      CC = CreditCurve5
    }
    TauMatrix_gaussian[,i] = HazardExactDefaultTime(CC,UMatrix_gaussian[,i])
  }
  
  LegCalculation_gaussian = matrix(data = NA,ncol=2, nrow=NumberSimulation)
  colnames(LegCalculation_gaussian) = c("DefaultLeg","PremiumLeg")
  #colnames(LegCalculation_gaussian) = c("DefaultLeg","PremiumLeg","DefaultLegCont","PremiumLegCont")
  
  #kth to default basket CDS
  for (i in seq(1,NumberSimulation)) {
    if ((i/NumberSimulation*100)%%25 == 0) cat((i/NumberSimulation)*100,"% ...\n")
    tau_list = sort(TauMatrix_gaussian[i,])
    tau_k = tau_list[k]
    
    #default leg calculation // in continuous time
    default_leg = 0
    if (tau_k == Inf) {
      #i.e. tau_k > 5. Number of default is below k during the life of the contract
      default_leg = 0
    }
    else {
      default_leg = (1-RecoveryRate)*GetDiscountFactor(DiscountCurve,tau_k) * (1/5)
    }
    
    #premium leg calculation // in disctrete time (yearly payment)
    #One coding solution is to create a variable that accumulates PL at each dt = 0.01 and will need a fiited discounting curve for this increment.
    #integrate GetDiscountFactor from 0 to min_tau
    premium_leg = 0
    if (tau_list[1] == Inf) {
      #i.e. No default within the life of the contract
      for (j in seq(1,5)) {
        premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,j)*1
      }
      premium_leg = premium_leg*(5/5)
    }
    else {
      premium_leg = compute_premium_leg(DiscountCurve,k,tau_list)
    }
    
    LegCalculation_gaussian[i,"DefaultLeg"] = default_leg
    LegCalculation_gaussian[i,"PremiumLeg"] = premium_leg
    
    
    #test in continuous tinme for k=1
    #LegCalculation_gaussian[i,"DefaultLegCont"] = (1-R)*GetDiscountFactor(DiscountCurve,tau_list[1])*(1/5)
    #LegCalculation_gaussian[i,"PremiumLegCont"] = GetDiscountFactor(DiscountCurve,tau_list[1])*(5/5)
  }
  
  print(cbind(TauMatrix_gaussian[,1],TauMatrix_gaussian[,2],TauMatrix_gaussian[,3],TauMatrix_gaussian[,4],TauMatrix_gaussian[,5],LegCalculation_gaussian[,"DefaultLeg"],LegCalculation_gaussian[,"PremiumLeg"]))
  
  expectation_default_leg_gaussian= mean(LegCalculation_gaussian[,"DefaultLeg"])
  expectation_premium_leg_gaussian= mean(LegCalculation_gaussian[,"PremiumLeg"])
  expectation_spread_gaussian = expectation_default_leg_gaussian/expectation_premium_leg_gaussian
  
  cat("======> expectation default leg:",mean(LegCalculation_gaussian[,"DefaultLeg"]),"\n")
  cat("======> expectation premium leg:",mean(LegCalculation_gaussian[,"PremiumLeg"]),"\n")
  #cat("======> expectation default leg (cont. time):",mean(LegCalculation_gaussian[,"DefaultLegCont"]),"\n")
  #cat("======> expectation premium leg (cont. time):",mean(LegCalculation_gaussian[,"PremiumLegCont"]),"\n")
  
  #convergence diagram
  nbobservation = round(NumberSimulation/10)
  expectation_default_leg_array = rep(NA,nbobservation)
  expectation_premium_leg_array = rep(NA,nbobservation)
  expectation_spread_array = rep(NA,nbobservation)
  
  for (i in seq(1,nbobservation)) {
    expectation_default_leg_array[i] = mean(LegCalculation_gaussian[1:i,"DefaultLeg"])
    expectation_premium_leg_array[i] = mean(LegCalculation_gaussian[1:i,"PremiumLeg"])
    expectation_spread_array[i] = expectation_default_leg_array[i]/expectation_premium_leg_array[i] 
  }
  plot(seq(1,nbobservation),expectation_spread_array, type="l", log="x")
  
  return(expectation_spread_gaussian*10000) #result is return in basis point
}