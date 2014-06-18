#Sampling from gaussian copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_GaussianCopula
A = chol(DefaultProbabilityMatrix_GaussianCopula)
#verification
t(A) %*% A

NumberCDS = 5
NumberSimulation = 300000
RecoveryRate = 0.40
YieldCurve = getYieldCurve(HistYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))

#ZMatrix = cbind(rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1))
#using sobol numbers
require(fOptions)
ZMatrix = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)

XMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
UMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
TauMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)

#we impose correlation
for (i in seq(1,NumberSimulation)) {
  XMatrix[i,] = t(A %*% ZMatrix[i,]) # t() in order to keep X as a row vector
}

UMatrix = pnorm(XMatrix)

for (i in seq(1,NumberCDS)) {
  CC = NULL
  cat("i=",i,"\n")
  if (i == 1) {
    CC = BootstrapHistoricCreditCurve(BMY_USD_XR_MARGINAL)[[1,"CreditCurve"]]
  }
  else if (i == 2) {
    CC = BootstrapHistoricCreditCurve(DELL_USD_XR_MARGINAL)[[1,"CreditCurve"]]
  }
  else if (i == 3) {
    CC = BootstrapHistoricCreditCurve(HP_USD_XR_MARGINAL)[[1,"CreditCurve"]]
  }
  else if (i == 4) {
    CC = BootstrapHistoricCreditCurve(IBM_USD_XR_MARGINAL)[[1,"CreditCurve"]]
  }
  else if (i == 5) {
    CC = BootstrapHistoricCreditCurve(PFE_USD_XR_MARGINAL)[[1,"CreditCurve"]]
  }
  TauMatrix[,i] = HazardExactDefaultTime(CC,UMatrix[,i])
}
TauMatrix

LegCalculation = matrix(data = NA,ncol=2, nrow=NumberSimulation)
colnames(LegCalculation) = c("DefaultLeg","PremiumLeg")

#1st to default basket CDS
for (i in seq(1,NumberSimulation)) {
  if (i%%(NumberSimulation/25) == 0) cat((i/NumberSimulation)*100,"% ...\n")
  min_tau = min(TauMatrix[i,])
  
  #default leg calculation
  default_leg = 0
  if (min_tau == Inf) {
    #i.e. min_tau > 5. No default within the life of the contract
    default_leg = 0
  }
  else {
    default_leg = (1-RecoveryRate)*GetDiscountFactor(YieldCurve,min_tau) * (1/5)
  }
  
  #premium leg calculation
  #One coding solution is to create a variable that accumulates PL at each dt = 0.01 and will need a fiited discounting curve for this increment.
  #integrate GetDiscountFactor from 0 to min_tau
  premium_leg = 0
  if (min_tau == Inf) {
    #i.e. min_tau > 5. No default within the life of the contract
    for (j in seq(1,5)) {
      premium_leg = premium_leg + GetDiscountFactor(YieldCurve,j)*1
    }
    #too slow using integrate
    #premium_leg = integrate(GetDiscountFactorVector,YieldCurve=YieldCurve,0,5)$value
    premium_leg = premium_leg*(5/5)
  }
  else {
    j=1
    while (j<min_tau & j<5) {
      premium_leg = premium_leg + GetDiscountFactor(YieldCurve,j)*1
      j = j+1
    }
    premium_leg = premium_leg + GetDiscountFactor(YieldCurve,min_tau)*(min_tau-(j-1))
    #too slow using integrate
    #premium_leg = integrate(GetDiscountFactorVector,YieldCurve=YieldCurve,0,min_tau)$value
    premium_leg = premium_leg*(5/5)
  }
  
  LegCalculation[i,"DefaultLeg"] = default_leg
  LegCalculation[i,"PremiumLeg"] = premium_leg  
}
LegCalculation

expectation_default_leg= mean(LegCalculation[,"DefaultLeg"])
expectation_premium_leg= mean(LegCalculation[,"PremiumLeg"])
expectation_spread = expectation_default_leg/expectation_premium_leg
expectation_spread

#convergence diagram
nbobservation = round(NumberSimulation/10)
expectation_default_leg_array = rep(NA,nbobservation)
expectation_premium_leg_array = rep(NA,nbobservation)
expectation_spread_array = rep(NA,nbobservation)

for (i in seq(1,nbobservation)) {
  expectation_default_leg_array[i] = mean(LegCalculation[1:i,"DefaultLeg"])
  expectation_premium_leg_array[i] = mean(LegCalculation[1:i,"PremiumLeg"])
  expectation_spread_array[i] = expectation_default_leg_array[i]/expectation_premium_leg_array[i] 
}
plot(seq(1,nbobservation),expectation_spread_array, type="l", log="x")

#kth to default basket CDS
k=1
for (i in seq(1,NumberSimulation)) {
  if (i%%(NumberSimulation/25) == 0) cat((i/NumberSimulation)*100,"% ...\n")
  tau_list = sort(TauMatrix[i,])
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
  
  LegCalculation[i,"DefaultLeg"] = default_leg
  LegCalculation[i,"PremiumLeg"] = premium_leg  
}
LegCalculation

expectation_default_leg= mean(LegCalculation[,"DefaultLeg"])
expectation_premium_leg= mean(LegCalculation[,"PremiumLeg"])
expectation_spread = expectation_default_leg/expectation_premium_leg
expectation_spread

#convergence diagram
nbobservation = round(NumberSimulation/10)
expectation_default_leg_array = rep(NA,nbobservation)
expectation_premium_leg_array = rep(NA,nbobservation)
expectation_spread_array = rep(NA,nbobservation)

for (i in seq(1,nbobservation)) {
  expectation_default_leg_array[i] = mean(LegCalculation[1:i,"DefaultLeg"])
  expectation_premium_leg_array[i] = mean(LegCalculation[1:i,"PremiumLeg"])
  expectation_spread_array[i] = expectation_default_leg_array[i]/expectation_premium_leg_array[i] 
}
plot(seq(1,nbobservation),expectation_spread_array, type="l", log="x")


truc = cbind(TauMatrix[1:250,1],TauMatrix[1:250,2],TauMatrix[1:250,3],TauMatrix[1:250,4],TauMatrix[1:250,5],LegCalculation[1:250,"DefaultLeg"],LegCalculation[1:250,"PremiumLeg"],LegCalculationBackup[1:250,"DefaultLeg"],LegCalculationBackup[1:250,"PremiumLeg"])

#k=1 ==> 82bp // 0.0082433965
#k=2 ==> 18bp
#k=3 ==> 3.5bp
#k=4 ==> 0.48bp

