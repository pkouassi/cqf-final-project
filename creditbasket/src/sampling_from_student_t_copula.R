#Sampling from student t copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_StudentTCopula
rho_matrix
degree_freedom

A = chol(DefaultProbabilityMatrix_StudentTCopula)
#verification
t(A) %*% A

NumberCDS = 5
NumberSimulation = 300000
RecoveryRate = 0.40
YieldCurve = getYieldCurve(HistYieldCurveMatrix,as.Date("23-MAY-2014","%d-%b-%Y"))