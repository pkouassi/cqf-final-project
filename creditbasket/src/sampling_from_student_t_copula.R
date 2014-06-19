#Sampling from student t copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_StudentTCopula
rho_matrix
degree_freedom

A_studentt = chol(DefaultProbabilityMatrix_StudentTCopula)
#verification
t(A_studentt) %*% A_studentt

NumberCDS = 5
NumberSimulation = 300000
RecoveryRate = 0.40
YieldCurve = getYieldCurve(HistYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))

require(fOptions)
ZMatrix = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
YMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
XMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
TauMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
ChiSquare = rchisq(NumberSimulation, degree_freedom)

#adjustment by the chi-squared number and apply correlation
for (i in seq(1,NumberSimulation)) {
  YMatrix[i,] = ZMatrix[i,]/(sqrt(ChiSquare[i]/degree_freedom))
  XMatrix[i,] = t(A_studentt %*% YMatrix[i,])# t() in order to keep X as a row vector
}

#convert to uniform correlated vector by applying the student t cdf
UMatrix = pt(XMatrix)

#convert U to Tau
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

#kth to default basket CDS
k=2
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


#k=1 ==> 0.0082775641
#k=2 ==> 0.0018593256
