#forward curve parsing
shortend_filename = "/../data/ukblc05_mdaily_fwdcurve_shortend.csv"
longend_filename = "/../data/ukblc05_mdaily_fwdcurve_longend.csv"
tt = parseHistoricalForwardCurve(shortend_filename,longend_filename)

#eigen values / vectors
#comparaison 
toto.values = EigenValuesVector(H,10^-16)
toto.vectors = EigenValuesMatrix(H,10^-16)

tata = eigen(H, FALSE, only.values = FALSE)
tata.values = tata$values
tata.vectors = tata$vectors

toto.values; tata.values
toto.vectors; tata.vectors

toto.vectors %*% diag(toto.values,3)  %*% solve(toto.vectors)
tata.vectors %*% diag(tata.values,3)  %*% solve(tata.vectors)

#2 methods equivalent when symetric

#debugging

r <- eigen(G)
V <- r$vectors
lam <- r$values
Lmbd =diag(lam)

V  %*% 

#eigen(CovarianceMatrix, TRUE, only.values = FALSE)$value
#sort(EigenValuesVector(CovarianceMatrix,10^-16),decreasing = TRUE)
#cbind(sort(EigenValuesVector(CovarianceMatrix,10^-25),decreasing = TRUE),eigen(CovarianceMatrix, TRUE, only.values = FALSE)$value)

#Matrix decomposition into Eigen vector and eigen values
#CovMatEigen = eigen(CovMat, TRUE, only.values = FALSE)
#CovMatEigenValues = CovMatEigen$values
#CovMatEigenVectors = CovMatEigen$vectors
#test = CovMatEigenVectors %*% diag(CovMatEigenValues)^2 %*% CovMatEigenVectors
#sort(CovMatEigenValues, decreasing = TRUE)
#cbind(Maturity,CovMatEigenValues)
#list = c()
#for (i in seq(1,nrow(CovMat))) {
#  list = c(list,CovMat[[i,i]])
#}
#CovMat

A = matrix( 
  c(2, 4, 3, 1), # the data elements 
  nrow=2,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

B = matrix( 
  c(2), # the data elements 
  nrow=1,              # number of rows 
  ncol=1,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

C = matrix( 
  c(2, 4, 3, 1,1, 5,8, 9,10), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

D = matrix( 
  c(2, 4, -3, 1,1, -5,8, 9,10), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 


E = matrix( 
  c(2, 1, 4, 1, 2, 4,0, 0,1), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

G = matrix( 
  c(2, 1, 10, 1, 2, 4,17, 51,1), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

H = matrix( 
  c(1, 0.33, 0.50, 0.33, 1, 0.60,0.5,0.6,1), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 



#Test trapezium rule
f = function(x) sqrt((2*x+1))
integral_trapezium_rule(f,0,1,4)
