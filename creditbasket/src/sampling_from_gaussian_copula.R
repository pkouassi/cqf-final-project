#Sampling from gaussian copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_GaussianCopula
A_gaussian = chol(DefaultProbabilityMatrix_GaussianCopula)
#verification
t(A_gaussian) %*% A_gaussian

NumberCDS = 5
NumberSimulation = 300000
RecoveryRate = 0.40
YieldCurve = getYieldCurve(HistoricalYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))

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
  cat("Converting u_i into tau_i for Asset",i,"\n")
  if (i == 1) {
    CC = BootstrapHistoricCreditCurve(CDS1_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 2) {
    CC = BootstrapHistoricCreditCurve(CDS2_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 3) {
    CC = BootstrapHistoricCreditCurve(CDS3_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 4) {
    CC = BootstrapHistoricCreditCurve(CDS4_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 5) {
    CC = BootstrapHistoricCreditCurve(CDS5_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  TauMatrix_gaussian[,i] = HazardExactDefaultTime(CC,UMatrix_gaussian[,i])
}
TauMatrix_gaussian

LegCalculation_gaussian = matrix(data = NA,ncol=2, nrow=NumberSimulation)
colnames(LegCalculation_gaussian) = c("DefaultLeg","PremiumLeg")

#kth to default basket CDS
k=1
for (i in seq(1,NumberSimulation)) {
  if (i%%(NumberSimulation/20) == 0) cat((i/NumberSimulation)*100,"% ...\n")
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
LegCalculation_gaussian

expectation_default_leg_gaussian= mean(LegCalculation_gaussian[,"DefaultLeg"])
expectation_premium_leg_gaussian= mean(LegCalculation_gaussian[,"PremiumLeg"])
expectation_spread_gaussian = expectation_default_leg_gaussian/expectation_premium_leg_gaussian
expectation_spread_gaussian

cat("expectation_spread_gaussian (generic algo):",expectation_spread_gaussian,"\n")

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



#k=1 ==> 82bp // 0.0082433965 
#k=2 ==> 18bp
#k=3 ==> 3.5bp
#k=4 ==> 0.48bp
