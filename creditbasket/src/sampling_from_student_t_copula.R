#Sampling from student t copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_StudentTCopula
degree_freedom
#degree_freedom = 7

A_studentt = chol(DefaultProbabilityMatrix_StudentTCopula)
#verification
t(A_studentt) %*% A_studentt

NumberCDS = 5
NumberSimulation = 300000
RecoveryRate = 0.40
YieldCurve = getYieldCurve(HistoricalYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))


ZMatrix_studentt = cbind(rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1))
#require(fOptions)
#ZMatrix_studentt = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
YMatrix_studentt = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
XMatrix_studentt = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
TauMatrix_studentt = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
ChiSquare = rchisq(NumberSimulation, degree_freedom)

#adjustment by the chi-squared number and apply correlation
for (i in seq(1,NumberSimulation)) {
  YMatrix_studentt[i,] = ZMatrix_studentt[i,]/(sqrt(ChiSquare[i]/degree_freedom))
  XMatrix_studentt[i,] = t(A_studentt %*% YMatrix_studentt[i,])# t() in order to keep X as a row vector
}

#convert to uniform correlated vector by applying the student t cdf
UMatrix_studentt = pt(XMatrix_studentt, df = degree_freedom)


#convert U to Tau
for (i in seq(1,NumberCDS)) {
  CC = NULL
  cat("i=",i,"\n")
  if (i == 1) {
    CC = BootstrapHistoricCreditCurve(BMY_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 2) {
    CC = BootstrapHistoricCreditCurve(DELL_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 3) {
    CC = BootstrapHistoricCreditCurve(HP_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 4) {
    CC = BootstrapHistoricCreditCurve(IBM_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  else if (i == 5) {
    CC = BootstrapHistoricCreditCurve(PFE_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
  }
  TauMatrix_studentt[,i] = HazardExactDefaultTime(CC,UMatrix_studentt[,i])
}
TauMatrix_studentt

LegCalculation_studentt = matrix(data = NA,ncol=2, nrow=NumberSimulation)
colnames(LegCalculation_studentt) = c("DefaultLeg","PremiumLeg")

#kth to default basket CDS
k=1
for (i in seq(1,NumberSimulation)) {
  if (i%%(NumberSimulation/25) == 0) cat((i/NumberSimulation)*100,"% ...\n")
  tau_list = sort(TauMatrix_studentt[i,])
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
  
  LegCalculation_studentt[i,"DefaultLeg"] = default_leg
  LegCalculation_studentt[i,"PremiumLeg"] = premium_leg  
}
LegCalculation_studentt

expectation_default_leg_studentt= mean(LegCalculation_studentt[,"DefaultLeg"])
expectation_premium_leg_studentt= mean(LegCalculation_studentt[,"PremiumLeg"])
expectation_spread_studentt = expectation_default_leg_studentt/expectation_premium_leg_studentt
expectation_spread_studentt

#convergence diagram
nbobservation = round(NumberSimulation/10)
expectation_default_leg_array = rep(NA,nbobservation)
expectation_premium_leg_array = rep(NA,nbobservation)
expectation_spread_array = rep(NA,nbobservation)

for (i in seq(1,nbobservation)) {
  expectation_default_leg_array[i] = mean(LegCalculation_studentt[1:i,"DefaultLeg"])
  expectation_premium_leg_array[i] = mean(LegCalculation_studentt[1:i,"PremiumLeg"])
  expectation_spread_array[i] = expectation_default_leg_array[i]/expectation_premium_leg_array[i] 
}
plot(seq(1,nbobservation),expectation_spread_array, type="l", log="x")


#k=1 ==> 0.008064315
#k=2 ==> 0.0018593256

