#Principal Component Analysis
#Data acquisition
Maturity = c(0.08, seq(0.5,25,by=0.5))

#input_hist_fwd_rate = read.csv("P:\\CQF\\FinalProject\\git-root\\finalproject\\interestrate-hjm\\data\\historical_forward_rate.csv",header = FALSE)
#HistoricalForwardCurve = data.matrix(input_hist_fwd_rate, rownames.force = NA)
#HistoricalForwardCurveDate = rep(as.Date("01 Jan 70","%d %b %y",nrow(HistoricalForwardCurve)-1))


#Forward Rates term structure plotting
plot_x_lim = c(0,25);
plot_y_lim = c(0,8);
matplot(Maturity,cbind(HistoricalForwardCurve[1,],HistoricalForwardCurve[500,],HistoricalForwardCurve[1000,]),type="l",col=c("blue","pink","yellow"),main="Forward Rates Term Structure",xlab="Maturity, year",ylab="Forward Curve",xlim=plot_x_lim, ylim=plot_y_lim,lwd=2,lty=1)
#plot(Maturity,HistoricalForwardCurve[2000,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="purple",
#     main="",xlab="",ylab="")

#return matrix calculation
HistoricalForwardReturn = matrix(NA, 
             nrow=(nrow(HistoricalForwardCurve)-1),
             ncol=ncol(HistoricalForwardCurve),
             byrow = TRUE);
HistoricalForwardReturnDate = rep(as.Date("01 Jan 70","%d %b %y",nrow(HistoricalForwardCurve)-1))

for (i in seq(1,nrow(HistoricalForwardCurve)-1)) {
  HistoricalForwardReturnDate[i] = HistoricalForwardCurveDate[i+1]
  HistoricalForwardReturn[i,] = HistoricalForwardCurve[i+1,] - HistoricalForwardCurve[i,]
}  
  
#covariance matrix calculation
#multiply by 252 to annualize the variance (we calculate daily return)
# divide by (100 * 100) because rates are expressed in percentage (i.e. 2 for 2% instead of 0.02)
CovarianceMatrix = cov(HistoricalForwardReturn) * 252 / (100*100)
eigen_values_list = EigenValuesVector(CovarianceMatrix,10^-20)
eigen_vectors_list = EigenValuesMatrix(CovarianceMatrix,10^-20)
eigen_values_list_sorted = sort(eigen_values_list,decreasing = TRUE)

PC1_lamda = eigen_values_list_sorted[1]
PC2_lamda = eigen_values_list_sorted[2]
PC3_lamda = eigen_values_list_sorted[3]

PC1_index = which(eigen_values_list == PC1_lamda)
PC2_index = which(eigen_values_list == PC2_lamda)
PC3_index = which(eigen_values_list == PC3_lamda)
  
cat("1st PC:",Maturity[PC1_index],"==> lambda=",PC1_lamda,", cumul weight (%):",(PC1_lamda/sum(eigen_values_list))*100,"\n")
cat("2nd PC:",Maturity[PC2_index],"==> lambda=",PC2_lamda,", cumul weight (%):",((PC1_lamda+PC2_lamda)/sum(eigen_values_list))*100,"\n")
cat("3rd PC:",Maturity[PC3_index],"==> lambda=",PC3_lamda,", cumul weight (%):",((PC1_lamda+PC2_lamda+PC3_lamda)/sum(eigen_values_list))*100,"\n")

PC1_eigen_vector = eigen_vectors_list[,PC1_index]
PC2_eigen_vector = eigen_vectors_list[,PC2_index]
PC3_eigen_vector = eigen_vectors_list[,PC3_index]

PC1_volatility = sqrt(PC1_lamda)*PC1_eigen_vector
PC2_volatility = sqrt(PC2_lamda)*PC2_eigen_vector
PC3_volatility = sqrt(PC3_lamda)*PC3_eigen_vector

PC1_volatility_fitted = function(x) {
  b0 = median(PC1_volatility)
  return(b0)
}

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

PC2_volatility_fitted = GetVolatilityFitFunction(PC2_volatility)
PC3_volatility_fitted = GetVolatilityFitFunction(PC3_volatility)

matplot(Maturity,cbind(PC1_volatility,sapply(Maturity,PC1_volatility_fitted)),type="l",col=c("blue","red"))
matplot(Maturity,cbind(PC2_volatility,sapply(Maturity,PC2_volatility_fitted)),type="l",col=c("blue","red"))
matplot(Maturity,cbind(PC3_volatility,sapply(Maturity,PC3_volatility_fitted)),type="l",col=c("blue","red"))
