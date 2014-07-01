BasketCDSPricing_GaussianCopula = function(CreditCurve1,CreditCurve2,CreditCurve3,CreditCurve4,CreditCurve5,YieldCurve,CorrelationMatrix,RecoveryRate,k=1,NumberSimulation=300000) {
  NumberCDS = 5
  #Test if correlation matrix is positive definite
  A_gaussian = chol(CorrelationMatrix)
  
  #ZMatrix_gaussian = cbind(rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1))
  #using sobol numbers
  ZMatrix_gaussian = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
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
  
  #kth to default basket CDS
  for (i in seq(1,NumberSimulation)) {
    if ((i/NumberSimulation*100)%%25 == 0) cat((i/NumberSimulation)*100,"% ...\n")
    tau_list = sort(TauMatrix_gaussian[i,])
    tau_k = tau_list[k]
    
    #default leg calculation
    default_leg = 0
    if (tau_k == Inf) {
      #i.e. tau_k > 5. Number of default is below k during the life of the contract
      default_leg = 0
    }
    else {
      default_leg = (1-RecoveryRate)*GetDiscountFactor(YieldCurve,tau_k) * (1/5)
    }
    
    #premium leg calculation
    #One coding solution is to create a variable that accumulates PL at each dt = 0.01 and will need a fiited discounting curve for this increment.
    #integrate GetDiscountFactor from 0 to min_tau
    premium_leg = 0
    if (tau_list[1] == Inf) {
      #i.e. No default within the life of the contract
      for (j in seq(1,5)) {
        premium_leg = premium_leg + GetDiscountFactor(YieldCurve,j)*1
      }
      premium_leg = premium_leg*(5/5)
    }
    else {
      premium_leg = compute_premium_leg(YieldCurve,k,tau_list)
    }
    
    LegCalculation_gaussian[i,"DefaultLeg"] = default_leg
    LegCalculation_gaussian[i,"PremiumLeg"] = premium_leg  
  }
  
  expectation_default_leg_gaussian= mean(LegCalculation_gaussian[,"DefaultLeg"])
  expectation_premium_leg_gaussian= mean(LegCalculation_gaussian[,"PremiumLeg"])
  expectation_spread_gaussian = expectation_default_leg_gaussian/expectation_premium_leg_gaussian
  
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
  
  return(expectation_spread_gaussian)
}