#Sampling from gaussian copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_GaussianCopula
A = chol(DefaultProbabilityMatrix_GaussianCopula)
#verification
t(A) %*% A

NumberCDS = 5
NumberSimulation = 300000

ZMatrix = cbind(rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1),rnorm(NumberSimulation, mean = 0, sd = 1))
XMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
UMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
TauMatrix = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)

for (i in seq(1,NumberSimulation)) {
  XMatrix[i,] = t(A %*% ZMatrix[i,]) # t() in order to keep X as a row vector
}

UMatrix = pnorm(XMatrix)

for (i in seq(1,NumberCDS)) {
  TauMatrix[,i] = HazardExactDefaultTime(BootstrapHistoricCreditCurve(BMY_USD_XR_MARGINAL)[[1,"CreditCurve"]],UMatrix[,i])
}

length(UMatrix[,i])

HazardExactDefaultTime(BootstrapHistoricCreditCurve(BMY_USD_XR_MARGINAL)[[1,"CreditCurve"]],UMatrix[,1])

#   U = pnorm(X)
#   Tau_1 = HazardExactDefaultTime(BootstrapHistoricCreditCurve(BMY_USD_XR_MARGINAL)[[1,"CreditCurve"]],U[1,1])
#   Tau_2 = HazardExactDefaultTime(BootstrapHistoricCreditCurve(DELL_USD_XR_MARGINAL)[[1,"CreditCurve"]],U[1,2])
#   Tau_3 = HazardExactDefaultTime(BootstrapHistoricCreditCurve(HP_USD_XR_MARGINAL)[[1,"CreditCurve"]],U[1,3])
#   Tau_4 = HazardExactDefaultTime(BootstrapHistoricCreditCurve(IBM_USD_XR_MARGINAL)[[1,"CreditCurve"]],U[1,4])
#   Tau_5 = HazardExactDefaultTime(BootstrapHistoricCreditCurve(PFE_USD_XR_MARGINAL)[[1,"CreditCurve"]],U[1,5])
#   #Tau = matrix(data = c(Tau_1,Tau_2,Tau_3,Tau_4,Tau_5),ncol=NumberCDS, nrow=1)
#   TauMatrix[i,] = c(Tau_1,Tau_2,Tau_3,Tau_4,Tau_5)
# }
# TauMatrix



