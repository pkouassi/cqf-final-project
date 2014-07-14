#==============================================================================
# title           :principal_component_analysis.R
# description     :Performs principal component analysis
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

Maturity = c(0.08, seq(0.5,25,by=0.5))

# Plot Forward Rates term structure
plot_x_lim = c(0,25);
plot_y_lim = c(0,5);
matplot(Maturity,cbind(HistoricalForwardCurve[1,],HistoricalForwardCurve[250,],HistoricalForwardCurve[500,],HistoricalForwardCurve[750,]),type="l",col=c("black","dodgerblue2","darkorchid1","chartreuse3"),xlab="Maturity, year",ylab="Forward Curve",xlim=plot_x_lim, ylim=plot_y_lim,lwd=1,lty=c(1,1,1,1))
legend(19, 1.6, c("June 2011","June 2012","June 2013","May 2014"), col = c("black","dodgerblue2","darkorchid1","chartreuse3"),
              text.col = c("black"), lty = c(1,1,1,1), pch = c(NA),
              merge = TRUE, bg = "gray90")
       

# Forward Log return matrix calculation
HistoricalForwardDifference = matrix(NA, 
             nrow=(nrow(HistoricalForwardCurve)-1),
             ncol=ncol(HistoricalForwardCurve),
             byrow = TRUE);
HistoricalForwardDifferenceDate = rep(as.Date("01 Jan 70","%d %b %y",nrow(HistoricalForwardCurve)-1))

for (i in seq(1,nrow(HistoricalForwardCurve)-1)) {
  HistoricalForwardDifferenceDate[i] = HistoricalForwardCurveDate[i+1]
  # we calculate log difference since we are in low interest rate regime since 2008
  HistoricalForwardDifference[i,] = log(HistoricalForwardCurve[i+1,]/HistoricalForwardCurve[i,])
}  
  
# Covariance matrix calculation
# multiply by 252 to annualize the variance (we calculate daily return)
# divide by (100 * 100) because rates are expressed in percentage (i.e. 2 for 2% instead of 0.02)
CovarianceMatrix = cov(HistoricalForwardDifference) * 252 / (100*100)
eigen_values_list = EigenValuesVector(CovarianceMatrix,10^-20)
eigen_vectors_list = EigenValuesMatrix(CovarianceMatrix,10^-20)
eigen_values_list_sorted = sort(eigen_values_list,decreasing = TRUE)

# Calculate PC eigenvalue
PC1_lamda = eigen_values_list_sorted[1]
PC2_lamda = eigen_values_list_sorted[2]
PC3_lamda = eigen_values_list_sorted[3]
PC4_lamda = eigen_values_list_sorted[4]
PC5_lamda = eigen_values_list_sorted[5]

# Obtain PC index
PC1_index = which(eigen_values_list == PC1_lamda)
PC2_index = which(eigen_values_list == PC2_lamda)
PC3_index = which(eigen_values_list == PC3_lamda)
PC4_index = which(eigen_values_list == PC4_lamda)
PC5_index = which(eigen_values_list == PC5_lamda)

# Print summary of the Principal components
cat("1st PC:",Maturity[PC1_index],"==> lambda=",PC1_lamda,"weight (%):",(PC1_lamda/sum(eigen_values_list))*100,", cumul weight (%):",(PC1_lamda/sum(eigen_values_list))*100,"\n")
cat("2nd PC:",Maturity[PC2_index],"==> lambda=",PC2_lamda,"weight (%):",(PC2_lamda/sum(eigen_values_list))*100,", cumul weight (%):",((PC1_lamda+PC2_lamda)/sum(eigen_values_list))*100,"\n")
cat("3rd PC:",Maturity[PC3_index],"==> lambda=",PC3_lamda,"weight (%):",(PC3_lamda/sum(eigen_values_list))*100,", cumul weight (%):",((PC1_lamda+PC2_lamda+PC3_lamda)/sum(eigen_values_list))*100,"\n")
cat("4th PC:",Maturity[PC4_index],"==> lambda=",PC4_lamda,"weight (%):",(PC4_lamda/sum(eigen_values_list))*100,", cumul weight (%):",((PC1_lamda+PC2_lamda+PC3_lamda+PC4_lamda)/sum(eigen_values_list))*100,"\n")
cat("5th PC:",Maturity[PC5_index],"==> lambda=",PC5_lamda,"weight (%):",(PC5_lamda/sum(eigen_values_list))*100,", cumul weight (%):",((PC1_lamda+PC2_lamda+PC3_lamda+PC4_lamda+PC5_lamda)/sum(eigen_values_list))*100,"\n")

# Define eigen vectors
PC1_eigen_vector = eigen_vectors_list[,PC1_index]
PC2_eigen_vector = eigen_vectors_list[,PC2_index]
PC3_eigen_vector = eigen_vectors_list[,PC3_index]
PC4_eigen_vector = eigen_vectors_list[,PC4_index]
PC5_eigen_vector = eigen_vectors_list[,PC5_index]

# Calculate PC volatility
PC1_volatility = sqrt(PC1_lamda)*PC1_eigen_vector
PC2_volatility = sqrt(PC2_lamda)*PC2_eigen_vector
PC3_volatility = sqrt(PC3_lamda)*PC3_eigen_vector
PC4_volatility = sqrt(PC4_lamda)*PC4_eigen_vector
PC5_volatility = sqrt(PC5_lamda)*PC5_eigen_vector

# Determine cubic spline fitted function for the PC volatility
# Function that perform multivariate linear regression on power of tau
GetVolatilityFitFunction = function(volatility) {
  Tau1 = Maturity
  Tau2 = Maturity^2
  Tau3 = Maturity^3
  fit = lm(volatility~Tau1+Tau2+Tau3)
  b0 = as.numeric(fit$coefficients["(Intercept)"])
  b1 = as.numeric(fit$coefficients["Tau1"])
  b2 = as.numeric(fit$coefficients["Tau2"])
  b3 = as.numeric(fit$coefficients["Tau3"])
  
  cat("b0=",b0," b1=",b1," b2=",b2," b3=",b3,"\n")
  
  func = function(x) {
    return(b0+b1*x+b2*x^2+b3*x^3)
  }
  return(func)
}

# Define the fitted function for the volatility of PC1
cat("b0=",median(PC1_volatility),"\n")
PC1_volatility_fitted = function(x) {
  b0 = median(PC1_volatility)
  return(b0)
}

# Define the fitted functions for the volatility of PC2/PC3/PC4/PC5
PC2_volatility_fitted = GetVolatilityFitFunction(PC2_volatility)
PC3_volatility_fitted = GetVolatilityFitFunction(PC3_volatility)
PC4_volatility_fitted = GetVolatilityFitFunction(PC4_volatility)
PC5_volatility_fitted = GetVolatilityFitFunction(PC5_volatility)

# Vectorized version of fitted function (required for integration)
PC1_volatility_fitted_vector = function(X){
  return(sapply(X,PC1_volatility_fitted))
}
PC2_volatility_fitted_vector = function(X){
  return(sapply(X,PC2_volatility_fitted))
}
PC3_volatility_fitted_vector = function(X){
  return(sapply(X,PC3_volatility_fitted))
}
PC4_volatility_fitted_vector = function(X){
  return(sapply(X,PC4_volatility_fitted))
}
PC5_volatility_fitted_vector = function(X){
  return(sapply(X,PC5_volatility_fitted))
}

# Plot the PC actual volatility
matplot(Maturity,100*cbind(PC1_eigen_vector,PC2_eigen_vector,PC3_eigen_vector,PC4_eigen_vector,PC5_eigen_vector),type="l", col=c("black","dodgerblue2","darkorchid1","gold2","green2"),lwd=1, lty=1,ylab="Eigen vectors",xlab="Maturity")
legend(21.2, 65, c("PC1","PC2","PC3","PC4","PC5"), col = c("black","dodgerblue2","darkorchid1","green2"),
       text.col = "black", lty = c(1,1,1,1), pch = c(NA),
       merge = TRUE, bg = "gray90")

# Plot PC actual volatility vs fitted function
layout(matrix(c(1,1,2,3,4,5), nrow=3, ncol=2, byrow = TRUE))
par(mar=c(2,2,2,2))
matplot(Maturity,cbind(PC1_volatility,sapply(Maturity,PC1_volatility_fitted)),type="l",col=c("blue","red"),xlab="Maturity",ylab="Volatility",main="PC1",lty=c(2,1))
legend(20, 0.005, c("Actual volatility","Fitted function"), col = c("blue","red"),
       text.col = "black", lty=c(2,1), pch = c(NA),
       merge = TRUE, bg = "gray90")
matplot(Maturity,cbind(PC2_volatility,sapply(Maturity,PC2_volatility_fitted)),type="l",col=c("blue","red"),xlab="Maturity",ylab="Volatility",main="PC2",lty=c(2,1))
matplot(Maturity,cbind(PC3_volatility,sapply(Maturity,PC3_volatility_fitted)),type="l",col=c("blue","red"),xlab="Maturity",ylab="Volatility",main="PC3",lty=c(2,1))
matplot(Maturity,cbind(PC4_volatility,sapply(Maturity,PC4_volatility_fitted)),type="l",col=c("blue","red"),xlab="Maturity",ylab="Volatility",main="PC4",lty=c(2,1))
matplot(Maturity,cbind(PC5_volatility,sapply(Maturity,PC5_volatility_fitted)),type="l",col=c("blue","red"),xlab="Maturity",ylab="Volatility",main="PC5",lty=c(2,1))
#restore par defaults
par(mfrow=c(1,1),mar=c(5, 4, 4, 2))


