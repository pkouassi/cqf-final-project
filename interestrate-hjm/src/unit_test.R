#forward curve parsing
shortend_filename = "/../data/ukblc05_mdaily_fwdcurve_shortend.csv"
longend_filename = "/../data/ukblc05_mdaily_fwdcurve_longend.csv"
tt = parseHistoricalForwardCurve(shortend_filename,longend_filename)

#eigen values / vectors
#comparaison 
EigenValuesVector(E,10^-16)
eigen(E, TRUE, only.values = FALSE)

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

#Test trapezium rule
f = function(x) sqrt((2*x+1))
integral_trapezium_rule(f,0,1,4)
