#==============================================================================
# title           :default_probability_correlation_matrix.R
# description     :calculate correlation matrix and degree of freedom
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# Bootstrap historical credit curves
CDS1_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS1_USD_XR,HistoricalYieldCurveMatrix)
CDS2_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS2_USD_XR,HistoricalYieldCurveMatrix)
CDS3_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS3_USD_XR,HistoricalYieldCurveMatrix)
CDS4_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS4_USD_XR,HistoricalYieldCurveMatrix)
CDS5_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS5_USD_XR,HistoricalYieldCurveMatrix)

# Convert Matrix to dataframe with 5Y/3Y Survival Probability and Default Probability for ease of calculation
CDS1_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS1_USD_XR_HistCreditCurve)
CDS2_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS2_USD_XR_HistCreditCurve)
CDS3_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS3_USD_XR_HistCreditCurve)
CDS4_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS4_USD_XR_HistCreditCurve)
CDS5_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS5_USD_XR_HistCreditCurve)

# Function that calculates survival probability / hazard rate differences
ComputeDifference = function(HistCreditCurveDataframe) {
  HistDifference = as.data.frame(matrix(ncol=9, nrow=(nrow(HistCreditCurveDataframe)-1)))
  names(HistDifference) = c("Date", "Ticker","DP_5Y_diff","DP_5Y_logdiff","HR_5Y","HR_5Y_diff","HR_5Y_logdiff","DP_3Y_logdiff","HR_3Y_logdiff")
  
  for (i in seq(2,nrow(HistCreditCurveDataframe))) {
    HistDifference$Date[i-1] = HistCreditCurveDataframe$Date[i]       
    HistDifference$Ticker[i-1] = HistCreditCurveDataframe$Ticker[i]
    
    #5Y Default Probability
    HistDifference$DP_5Y_diff[i-1] = (HistCreditCurveDataframe$DP_5Y[i] - HistCreditCurveDataframe$DP_5Y[i-1])    
    HistDifference$DP_5Y_logdiff[i-1] = log(HistCreditCurveDataframe$DP_5Y[i]/HistCreditCurveDataframe$DP_5Y[i-1])
    #5Y Hazard Rate
    HistDifference$HR_5Y[i-1] = HistCreditCurveDataframe$HR_5Y[i-1]
    HistDifference$HR_5Y_diff[i-1] = (HistCreditCurveDataframe$HR_5Y[i] - HistCreditCurveDataframe$HR_5Y[i-1]) 
    HistDifference$HR_5Y_logdiff[i-1] = log(HistCreditCurveDataframe$HR_5Y[i]/HistCreditCurveDataframe$HR_5Y[i-1])
    
    #3Y Default Probability and Hazard Rate
    HistDifference$DP_3Y_logdiff[i-1] = log(HistCreditCurveDataframe$DP_3Y[i]/HistCreditCurveDataframe$DP_3Y[i-1])
    HistDifference$HR_3Y_logdiff[i-1] = log(HistCreditCurveDataframe$HR_3Y[i]/HistCreditCurveDataframe$HR_3Y[i-1])    
  }
  return(HistDifference)
}

CDS1_USD_XR_diff = ComputeDifference(CDS1_USD_XR_HistCreditCurveDataframe)
CDS2_USD_XR_diff = ComputeDifference(CDS2_USD_XR_HistCreditCurveDataframe)
CDS3_USD_XR_diff = ComputeDifference(CDS3_USD_XR_HistCreditCurveDataframe)
CDS4_USD_XR_diff = ComputeDifference(CDS4_USD_XR_HistCreditCurveDataframe)
CDS5_USD_XR_diff = ComputeDifference(CDS5_USD_XR_HistCreditCurveDataframe)

# Plot 5Y Default Probability for CDS1 to identify outliers
plot(CDS1_USD_XR_diff$Date,CDS1_USD_XR_diff$DP_5Y_diff,type="l",main="",ylab="5Y default probability difference",xlab="date")

# Plot histograms for Survival probability difference / hazard rate difference
hist(CDS1_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS1_USD_XR_diff$DP_5Y_logdiff, breaks=30)
hist(CDS1_USD_XR_diff$HR_5Y, breaks=30)
hist(CDS1_USD_XR_diff$HR_5Y_diff, breaks=30)
hist(CDS1_USD_XR_diff$HR_5Y_logdiff, breaks=30)
hist(CDS2_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS3_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS4_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS5_USD_XR_diff$DP_5Y_diff, breaks=30)

#==============================================================================
# Gaussian Copulae - Correlation Matrix
#==============================================================================

# Transform a data set to normal using the empirical cdf function ecdf()
transform_to_normal = function(X) {
  fn = ecdf(X) 
  U = fn(X)
  Z = qnorm(U)
  return(Z)
}

# Calculate Pearson correlation
# exclude outliers which fail the correlation calculation
pearson_correlation = function(Z1,Z2) {

  if (length(Z1) != length(Z2)) {
    warning("Two vectors have different length. can't compute pearson correlation")
    return()
  }
  else {
    Z1_observation_to_be_removed = rep(TRUE,length(Z1))
    Z2_observation_to_be_removed = rep(TRUE,length(Z2))
    
    for (i in seq(1,length(Z1))) {
      if (Z1[i] == Inf | is.na(Z1[i])) Z1_observation_to_be_removed[i] = FALSE
      if (Z2[i] == Inf | is.na(Z2[i])) Z2_observation_to_be_removed[i] = FALSE
    }
    
    return(cor(Z1[Z1_observation_to_be_removed & Z2_observation_to_be_removed],Z2[Z1_observation_to_be_removed & Z2_observation_to_be_removed], method = "pearson"))
  }
}

# Calculate a correlation matrix using pearson correlation
pearson_correlation_matrix = function(matrix) {
  CorrelationMatrix = matrix(NA, 
                  nrow=nrow(matrix),
                  ncol=nrow(matrix),
                  byrow = TRUE); 
  rownames(CorrelationMatrix) = AssetTicker 
  colnames(CorrelationMatrix) = AssetTicker 
  
  for (i in seq(1,nrow(matrix))) {
    for (j in seq(1,nrow(matrix))) {
      CorrelationMatrix[i,j] = pearson_correlation(matrix[i,],matrix[j,])
    }
  }
  
  return(CorrelationMatrix)  
}



# Correlation Matrix for Gaussian copula (Difference of Default Probability - 5Y)
DP_5Y_diff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$DP_5Y_diff),
  transform_to_normal(CDS2_USD_XR_diff$DP_5Y_diff),
  transform_to_normal(CDS3_USD_XR_diff$DP_5Y_diff),
  transform_to_normal(CDS4_USD_XR_diff$DP_5Y_diff),
  transform_to_normal(CDS5_USD_XR_diff$DP_5Y_diff)))

DP_5Y_diff_CorrelationMatrix_GaussianCopula

hist(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_diff), breaks=30)
hist(transform_to_normal(CDS2_USD_XR_diff$DP_5Y_diff), breaks=30)
hist(transform_to_normal(CDS3_USD_XR_diff$DP_5Y_diff), breaks=30)

# Correlation Matrix for Gaussian copula (Log Difference of Default Probability - 5Y)
DP_5Y_logdiff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS2_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS3_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS4_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS5_USD_XR_diff$DP_5Y_logdiff)))

hist(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff), breaks=30)

DP_5Y_logdiff_CorrelationMatrix_GaussianCopula
CorrelationMatrix_GaussianCopula = DP_5Y_logdiff_CorrelationMatrix_GaussianCopula

# Rolling 60days correlation (Log Difference of Default Probability - 5Y)
lag = 60
rolling_correlation_1vs5 = rep(NA,length(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff))-lag)
rolling_correlation_3vs4 = rep(NA,length(transform_to_normal(CDS3_USD_XR_diff$DP_5Y_logdiff))-lag)
asset1 = transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff)
asset2 = transform_to_normal(CDS2_USD_XR_diff$DP_5Y_logdiff)
asset3 = transform_to_normal(CDS3_USD_XR_diff$DP_5Y_logdiff)
asset4 = transform_to_normal(CDS4_USD_XR_diff$DP_5Y_logdiff)
asset5 = transform_to_normal(CDS5_USD_XR_diff$DP_5Y_logdiff)
for (i in seq(1,length(rolling_correlation_1vs5))) {
  rolling_correlation_1vs5[i] = pearson_correlation(asset1[i:(i+(lag-1))],asset5[i:(i+(lag-1))])
  rolling_correlation_3vs4[i] = pearson_correlation(asset3[i:(i+(lag-1))],asset4[i:(i+(lag-1))])  
}

matplot(seq(1,length(rolling_correlation_1vs5)),cbind(rolling_correlation_1vs5,rolling_correlation_3vs4),type="l",col=c("black","blue"),lty=1,xlab="Time",ylab="Correlation",ylim=c(0,0.75))
legend(68.5, 0.14, c("Corr(BMY,PFE)","Corr(HP,IBM)"), col = c("black","blue"), text.col = "black", lty = c(1,1), pch = c(NA),
merge = TRUE, bg = "gray90")

# correlation stability
lag_70=70
rolling_correlation_1vs5_lag_70 = rep(NA,length(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff))-lag_70)
lag_90=130
rolling_correlation_1vs5_lag_90 = rep(NA,length(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff))-lag_70)
lag_110=170
rolling_correlation_1vs5_lag_110 = rep(NA,length(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff))-lag_70)

for (i in seq(1,length(rolling_correlation_1vs5))) {
  rolling_correlation_1vs5_lag_70[i] = pearson_correlation(asset1[i:(i+(lag_70-1))],asset5[i:(i+(lag_70-1))])
  rolling_correlation_1vs5_lag_90[i] = pearson_correlation(asset1[i:(i+(lag_90-1))],asset5[i:(i+(lag_90-1))])
  rolling_correlation_1vs5_lag_110[i] = pearson_correlation(asset1[i:(i+(lag_110-1))],asset5[i:(i+(lag_110-1))])
}

matplot(cbind(rolling_correlation_1vs5_lag_70,rolling_correlation_1vs5_lag_90,rolling_correlation_1vs5_lag_110),type="l",lty=1)


# Correlation Matrix for Gaussian copula (Hazard Rate - 5Y)
HR_5Y_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS2_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS3_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS4_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS5_USD_XR_diff$HR_5Y)))

HR_5Y_CorrelationMatrix_GaussianCopula


# Correlation Matrix for Gaussian copula (Difference of Hazard Rate - 5Y)
HR_5Y_diff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS2_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS3_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS4_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS5_USD_XR_diff$HR_5Y_diff)))

HR_5Y_diff_CorrelationMatrix_GaussianCopula

# Correlation Matrix for Gaussian copula (Log Difference of Hazard Rate - 5Y)
HR_5Y_logdiff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS2_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS3_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS4_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS5_USD_XR_diff$HR_5Y_logdiff)))

HR_5Y_logdiff_CorrelationMatrix_GaussianCopula

# Rolling 60days correlation (Log Difference of Hazard rates - 5Y)
lag = 60
rolling_correlation_1vs5 = rep(NA,length(transform_to_normal(CDS1_USD_XR_diff$HR_5Y_logdiff))-lag)
rolling_correlation_3vs4 = rep(NA,length(transform_to_normal(CDS3_USD_XR_diff$HR_5Y_logdiff))-lag)
asset1 = transform_to_normal(CDS1_USD_XR_diff$HR_5Y_logdiff)
asset2 = transform_to_normal(CDS2_USD_XR_diff$HR_5Y_logdiff)
asset3 = transform_to_normal(CDS3_USD_XR_diff$HR_5Y_logdiff)
asset4 = transform_to_normal(CDS4_USD_XR_diff$HR_5Y_logdiff)
asset5 = transform_to_normal(CDS5_USD_XR_diff$HR_5Y_logdiff)
for (i in seq(1,length(rolling_correlation_1vs5))) {
  rolling_correlation_1vs5[i] = pearson_correlation(asset1[i:(i+(lag-1))],asset5[i:(i+(lag-1))])
  rolling_correlation_3vs4[i] = pearson_correlation(asset3[i:(i+(lag-1))],asset4[i:(i+(lag-1))])
}
matplot(seq(1,length(rolling_correlation_1vs5)),cbind(rolling_correlation_1vs5,rolling_correlation_3vs4),type="l",col=c("black","blue"),lty=1,xlab="Time",ylab="Correlation",ylim=c(0,0.75))
legend(305, 0.14, c("Corr(BMY,PFE)","Corr(HP,IBM)"), col = c("black","blue"), text.col = "black", lty = c(1,1), pch = c(NA),
       merge = TRUE, bg = "gray90")


#==============================================================================
# Student's T Copulae - Correlation Matrix
#==============================================================================

# Trasnform a data set to uniform, use the empirical cdf function ecdf()
# Since data is close to normal, ecdf should give acceptable results
transform_to_uniform = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
  fn = ecdf(X) 
  U = fn(X)
  return(U)
}

# Calculate Kendall Correlation (rank correlation measure)
kendall_correlation = function(Z1,Z2) {  
  if (length(Z1) != length(Z2)) {
    warning("Two vectors have different length. can't compute kendall correlation")
    return()
  }
  else {
    Z1_observation_to_be_removed = rep(TRUE,length(Z1))
    Z2_observation_to_be_removed = rep(TRUE,length(Z2))
    
    for (i in seq(1,length(Z1))) {
      if (Z1[i] == Inf | is.na(Z1[i])) Z1_observation_to_be_removed[i] = FALSE
      if (Z2[i] == Inf | is.na(Z2[i])) Z2_observation_to_be_removed[i] = FALSE
    }
    
    return(cor(Z1[Z1_observation_to_be_removed & Z2_observation_to_be_removed],Z2[Z1_observation_to_be_removed & Z2_observation_to_be_removed], method = "kendall"))
  }
}

# Calculate a correlation matrix using Kendall Correlation
kendall_correlation_matrix = function(matrix) {
  CorrelationMatrix = matrix(NA, 
                             nrow=nrow(matrix),
                             ncol=nrow(matrix),
                             byrow = TRUE); 
  rownames(CorrelationMatrix) = AssetTicker 
  colnames(CorrelationMatrix) = AssetTicker 
  
  for (i in seq(1,nrow(matrix))) {
    for (j in seq(1,nrow(matrix))) {
      CorrelationMatrix[i,j] = kendall_correlation(matrix[i,],matrix[j,])
    }
  }
  
  return(CorrelationMatrix)  
}


# Verify the result of our transform_to_uniform() function
# We obtain reasonably good "uniform" result with ecdf
hist(transform_to_uniform(CDS1_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS2_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS3_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS4_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS5_USD_XR_diff$DP_5Y_logdiff),breaks = 30)

# Calculate Kendall Tau correlation matrix
CorrelationMatrix_KendallTau = kendall_correlation_matrix(rbind(
  transform_to_uniform(CDS1_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS2_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS3_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS4_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS5_USD_XR_diff$DP_5Y_logdiff)
  ))



# Infer linear correlation matrix
# convert kendall tau coeedficient to normal cooefficient
# correlation matrix to be fed into copula 
# (copula density fitting and sampling from copula procedure)
CorrelationMatrix_StudentTCopula = sin(0.5*pi*CorrelationMatrix_KendallTau)
rownames(CorrelationMatrix_StudentTCopula) = AssetTicker 
colnames(CorrelationMatrix_StudentTCopula) = AssetTicker 


#==============================================================================
# Student's T Copulae - Degree of Freedom
#==============================================================================



# Define Student's t copula density function
C = function (U,v,Sigma) {
  n = length(U)
  
  first_part = (1/sqrt(det(Sigma)))*(gamma((v+n)/2)/gamma(v/2))*(gamma(v/2)/gamma((v+1)/2))^n
  
  numerator = (1 + ((qt(t(U), df=v) %*% solve(Sigma) %*% qt(U, df=v))/v))^(-(v+n)/2)
  denominator = 1
  for (i in seq(1,n)) {
    denominator = denominator * (1+((qt(U[i],df=v)^2)/v))^(-(v+1)/2) 
  } 
  second_part = numerator/denominator
  
  return(first_part*second_part)
}

# Define Log likelihood function 
loglikelihoodfunc = function(UMatrix,v,Sigma) {
  value = 0
  for (i in seq(1,nrow(UMatrix))) {
    value = value + log(C(UMatrix[i,],v,Sigma))
    #cat("[i=",i,"] =>",value,"\n")
  }
  return(value)
}

# Suppress observation where one or more ui = 1.00; these elements crash the MLE
U_array = cbind(transform_to_uniform(CDS1_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS2_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS3_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS4_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS5_USD_XR_diff$DP_5Y_logdiff))

U_array = U_array[(U_array[,1]!=1.0 & U_array[,2]!=1.0 & U_array[,3]!=1.0 & U_array[,4]!=1.0 & U_array[,5]!=1.0) &
                  !is.na(U_array[,1]) & !is.na(U_array[,2]) & !is.na(U_array[,3]) & !is.na(U_array[,4]) & !is.na(U_array[,5]),]

# Plot of the log-likelihood function
v_array = seq(2,60,by=1)
loglikelihood_array = sapply(v_array,loglikelihoodfunc,UMatrix=U_array,Sigma=CorrelationMatrix_StudentTCopula)
plot(v_array,loglikelihood_array,type="l",xlab="Degrees of freedom",ylab="Log-likelyhood",log="y",ylim=c(105,115))

# Maximum Likelihood estimation to find degree of freedom
# optimization routine to compute degree of freedom for student t
degree_freedom = optimize(loglikelihoodfunc, UMatrix = U_array, Sigma = CorrelationMatrix_StudentTCopula, interval=c(1, 50),maximum=TRUE)$maximum

