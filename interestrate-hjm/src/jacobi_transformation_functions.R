#==============================================================================
# title           :market_data_loading.R
# description     :Defines function used in Jacobi transformation
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# Calculate the sum of the square of the element of the upper triangle part of the matrix
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

# Calculate the Jacobi rotation parameter
JacobiRotationParameter = function (matrix) {
  # Returns vector containing mr, mc and jrad
  # These are the row and column vectors and the angle of rotation for the P matrix
  max_value = -1
  max_row = -1
  max_col = -1
  rotation_angle = 0
  
  # Take the absolute value of all matrix element
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
  
  # calculate rotation
  if (matrix[max_row,max_row] == matrix[max_col,max_col]) {
    # Stability check for rotation angle as Pi/4 - tan(2phi) will explode to infinity
    rotation_angle = 0.25 * pi * sign(matrix[max_row,max_col])
  }
  else {
    # Rotation angle calculation using tan(2phi)
    rotation_angle = 0.5 * atan(2 * matrix[max_row,max_col] / (matrix[max_row,max_row]-matrix[max_col,max_col]))
  }
  
  result = c(max_row,max_col,rotation_angle)
  names(result) <- c("max_row","max_col","rotation_angle")
  return(result)
}

# Construct Jacobi rotation matrix
JacobiRotationMatrix = function(param,size) {
  result = diag(size)
  result[param["max_row"],param["max_row"]] = cos(param["rotation_angle"])
  result[param["max_col"],param["max_row"]] = sin(param["rotation_angle"])
  result[param["max_row"],param["max_col"]] = -sin(param["rotation_angle"])
  result[param["max_col"],param["max_col"]] = cos(param["rotation_angle"])
  return(result)
}

JacobiA = function(A, size) {
  # Define P
  rotation_param = JacobiRotationParameter(A)
  P = JacobiRotationMatrix(rotation_param,nrow(A))
  # Rotation occurs below as A'=P'x A x P
  A_next = t(P) %*% (A %*% P)
  return(A_next)
}

JacobiV = function(A, V, size) {
  # Define P
  # Search for the largest off-diagonal element to be eliminated by rotation, generate the angle of rotation
  # Generate rotation matrix P
  P = JacobiRotationMatrix(JacobiRotationParameter(A),nrow(A))
  
  # Improving eigenvectors by Vi x Pi, where V reflects previous multipliations
  V_next = V %*% P
  return(V_next)
}

# Calculate eigenvalues
EigenValuesVector = function (A,tolerance) {
  current_A = A
  n = nrow(A)
  result = rep(NA, n)
  sumsquare = UpperTriangleSumSquare(current_A)
  k = 0
  while (sumsquare > tolerance) {    
    next_A = JacobiA(current_A,n)
    sumsquare = UpperTriangleSumSquare(next_A)
    # recurrence
    current_A = next_A
    k = k+1
  }
  
  for (i in seq(1,n)) {
    result[i] = current_A[i,i]
  }
  
  return(result)  
}

# Calculate eigenvectors
EigenValuesMatrix = function (A,tolerance) {
  current_A = A
  n = nrow(A)
  current_V = diag(n)
  sumsquare = UpperTriangleSumSquare(current_A)
  k = 0
  while (sumsquare > tolerance) {    
    next_A = JacobiA(current_A,n)
    next_V = JacobiV(current_A,current_V,n)
    
    sumsquare = UpperTriangleSumSquare(next_A)
    # recurrence
    current_A = next_A
    current_V = next_V
    k = k+1
  }
  
  result = next_V
  return(result)  
}
