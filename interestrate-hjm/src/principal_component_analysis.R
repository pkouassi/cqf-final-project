#Principal Component Analysis
#Data acquisition
input_hist_fwd_rate = read.csv("P:\\CQF\\FinalProject\\git-root\\finalproject\\interestrate-hjm\\data\\historical_forward_rate.csv",header = FALSE)

Maturity = c(0.08, seq(0.5,25,by=0.5))
HistFwdRateMat = data.matrix(input_hist_fwd_rate, rownames.force = NA)

#Forward Rates term structure plotting
plot_x_lim = c(0,25);
plot_y_lim = c(0,8);
plot(Maturity,HistFwdRateMat[1,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="blue", main="Forward Rates Term Structure",xlab="Maturity, year",ylab="")
par(new=TRUE);
plot(Maturity,HistFwdRateMat[500,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="pink",
     main="",xlab="",ylab="")
par(new=TRUE);
plot(Maturity,HistFwdRateMat[1000,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="yellow",
     main="",xlab="",ylab="")


#return matrix calculation
HistFwdReturnMat = matrix(NA, 
             nrow=(nrow(HistFwdRateMat)-1),
             ncol=ncol(HistFwdRateMat),
             byrow = TRUE);

for (i in seq(1,(nrow(HistFwdRateMat)-1))) {
  for (j in seq(1,ncol(HistFwdRateMat))) {
    HistFwdReturnMat[i,j] = HistFwdRateMat[i+1,j]-HistFwdRateMat[i,j]
  }
}

#covariance matrix calculation
CovMat = cov(HistFwdReturnMat) * (252/(100*100))

EigenValuesVector(CovMat,10^-16)
EigenValuesMatrix(CovMat,10^-16)
eigen(CovMat, TRUE, only.values = FALSE)

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


UpperTriangleSumSquare = function (matrix) {
  #Sum of squares fo the upper triangle part ot the a matrix
  sum = 0
  if (nrow(matrix) == ncol(matrix) && nrow(matrix) > 1 ) {
    for (i in seq(1,nrow(matrix)-1)) {
      for (j in seq(i+1,ncol(matrix))) {
        sum = sum + (matrix[i,j]^2)
      }
    }
    UpperTriangleSumSquare = sum
  }
  else if (nrow(matrix) == ncol(matrix) && nrow(matrix) == 1 ) {
    UpperTriangleSumSquare = matrix[1,1]^2
  }
  else {
    cat("Error: Matrix is not square [nrow=",nrow(matrix),",ncol=",ncol(matrix),"]\n")
    UpperTriangleSumSquare = -1
  }
}

JacobiRotationParameter = function (matrix) {
  #Returns vector containing mr, mc and jrad
  #These are the row and column vectors and the angle of rotation for the P matrix
  max_value = -1
  max_row = -1
  max_col = -1
  rotation_angle = 0
  
  #take the absolute value of all matrix element
  tmpMatrix = abs(matrix)
  
  for (i in seq(1,nrow(matrix)-1)) {
    for (j in seq(i+1,ncol(matrix))) {
      if (tmpMatrix[i,j] > max_value) {
        max_value = tmpMatrix[i,j]
        max_row = i
        max_col = j
      }
    }
  }
  
  #calculate rotation
  if (matrix[max_row,max_row] == matrix[max_col,max_col]) {
    #Stability check for rotation angle as Pi/4 - tan(2phi) will explode to infinity
    rotation_angle = 0.25 * pi * sign(matrix[max_row,max_col])
  }
  else {
    #Rotation angle calculation using tan(2phi)
    rotation_angle = 0.5 * atan(2 * matrix[max_row,max_col] / (matrix[max_row,max_row]-matrix[max_col,max_col]))
  }
  
  result = c(max_row,max_col,rotation_angle)
  names(result) <- c("max_row","max_col","rotation_angle")
  return(result)
}

JacobiRotationMatrix = function(param,size) {
  result = diag(size)
  result[param["max_row"],param["max_row"]] = cos(param["rotation_angle"])
  result[param["max_col"],param["max_row"]] = sin(param["rotation_angle"])
  result[param["max_row"],param["max_col"]] = -sin(param["rotation_angle"])
  result[param["max_col"],param["max_col"]] = cos(param["rotation_angle"])
  return(result)
}

JacobiA = function(A, size) {
  #define P
  P = JacobiRotationMatrix(JacobiRotationParameter(A),nrow(A))
  #Rotation occurs below as A'=P'x A x P
  A_next = t(P) %*% (A %*% P)
  return(A_next)
}

JacobiV = function(A, V, size) {
  #define P
  #Search for the largest off-diagonal element to be eliminated by rotation, generate the angle of rotation
  #Generate rotation matrix P
  P = JacobiRotationMatrix(JacobiRotationParameter(A),nrow(A))
  
  #Improving eigenvectors by Vi x Pi, where V reflects previous multipliations
  V_next = V %*% P
  return(V_next)
}


EigenValuesVector = function (A,tolerance) {
  current_A = A
  n = nrow(A)
  result = rep(NA, n)
  sumsquare = UpperTriangleSumSquare(current_A)
  k = 0
  while (sumsquare > tolerance) {
    #cat("--------------\n")
    #cat("k=",k,"\n")
    #cat("sumsquare=",sumsquare,"\n")
    
    next_A = JacobiA(current_A,n)
    sumsquare = UpperTriangleSumSquare(next_A)
    #recurrence
    current_A = next_A
    k = k+1
  }
  
  for (i in seq(1,n)) {
    result[i] = current_A[i,i]
  }
  
  return(result)  
}

EigenValuesMatrix = function (A,tolerance) {
  current_A = A
  n = nrow(A)
  current_V = diag(n)
  sumsquare = UpperTriangleSumSquare(current_A)
  k = 0
  while (sumsquare > tolerance) {
    #cat("--------------\n")
    #cat("k=",k,"\n")
    #cat("sumsquare=",sumsquare,"\n")
    
    next_A = JacobiA(current_A,n)
    next_V = JacobiV(current_A,current_V,n)
    sumsquare = UpperTriangleSumSquare(next_A)
    #recurrence
    current_A = next_A
    current_V = next_V
    k = k+1
  }
  
  result = next_V
  return(result)  
}

#comparaison
EigenValuesVector(E,10^-16)
eigen(E, TRUE, only.values = FALSE)
  

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

