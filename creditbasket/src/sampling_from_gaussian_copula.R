#Sampling from gaussian copula

#Test if correlation matrix is positive definite
DefaultProbabilityMatrix_GaussianCopula
A = chol(DefaultProbabilityMatrix_GaussianCopula)
#verification
t(A) %*% A

NumberCDS = 5
Z = rnorm(NumberCDS, mean = 0, sd = 1)
X = A %*% Z
U = pnorm(X)


