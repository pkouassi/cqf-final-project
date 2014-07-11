#arrays of CreditCurve Objects
CDS1_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS1_USD_XR,HistoricalYieldCurveMatrix)
CDS2_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS2_USD_XR,HistoricalYieldCurveMatrix)
CDS3_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS3_USD_XR,HistoricalYieldCurveMatrix)
CDS4_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS4_USD_XR,HistoricalYieldCurveMatrix)
CDS5_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(CDS5_USD_XR,HistoricalYieldCurveMatrix)

#dataframe with 5Y Survival Probability and Default Probability
CDS1_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS1_USD_XR_HistCreditCurve)
CDS2_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS2_USD_XR_HistCreditCurve)
CDS3_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS3_USD_XR_HistCreditCurve)
CDS4_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS4_USD_XR_HistCreditCurve)
CDS5_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(CDS5_USD_XR_HistCreditCurve)

#calculate default probability difference
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

#represent plot
plot(CDS1_USD_XR_diff$Date,CDS1_USD_XR_diff$DP_5Y_diff,type="l",main="",ylab="5Y default probability difference",xlab="date")
#CDS1_USD_XR_PDdiff$DP_5Y_change[33]

#represent histograms
hist(CDS1_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS1_USD_XR_diff$DP_5Y_logdiff, breaks=30)
hist(CDS1_USD_XR_diff$HR_5Y, breaks=30)
hist(CDS1_USD_XR_diff$HR_5Y_diff, breaks=30)
hist(CDS1_USD_XR_diff$HR_5Y_logdiff, breaks=30)

hist(CDS2_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS3_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS4_USD_XR_diff$DP_5Y_diff, breaks=30)
hist(CDS5_USD_XR_diff$DP_5Y_diff, breaks=30)


#cbind(CDS1_USD_XR_PDdiff$DP_5Y_change,CDS2_USD_XR_PDdiff$DP_5Y_change,CDS3_USD_XR_PDdiff$DP_5Y_change,CDS4_USD_XR_PDdiff$DP_5Y_change,CDS5_USD_XR_PDdiff$DP_5Y_change)

#correlation for gaussian copula
transform_to_normal = function(X) {
  fn = ecdf(X) 
  U = fn(X)
  Z = qnorm(U)
  return(Z)
}

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
  
  #check for Inf
  #spikes_for_Z1 = !(Z1 != Inf)
  #spikes_for_Z2 = !(Z2 != Inf)
  #spikes_for_both = spikes_for_Z1 | spikes_for_Z2  
  #cat(sum(spikes_for_both == TRUE)," date(s) excluded in both time series...\n")
  #return(cor(Z1[spikes_for_both==FALSE],Z2[spikes_for_both==FALSE], method = "pearson"))
}

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


#----------------------------------------------------------------------------------
#Correlation Matrix for Gaussian copula (Difference of Default Probability - 5Y)
#----------------------------------------------------------------------------------
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

#par(mfrow=c(1,2))
#plot(density(CDS1_USD_XR_diff$DP_5Y_diff[!is.na(CDS1_USD_XR_diff$DP_5Y_diff)]),main="5Y Def. Prob. (diff)",ylab="",xlab="", col="black",lty=1)
#plot(density(CDS1_USD_XR_diff$DP_5Y_logdiff[!is.na(CDS1_USD_XR_diff$DP_5Y_logdiff)]),main="5Y Def. Prob. (log diff)",ylab="",xlab="",col="black",lty=1)

# legend(0.02, 25, c("5Y Default Probability difference","5Y Default Probability log difference"), col = c("black"),
#       text.col = "black", lty = c(1,2), pch = c(NA),
#       merge = TRUE, bg = "gray90")

#matplot(cbind(density(CDS1_USD_XR_diff$DP_5Y_diff)$y,density(CDS1_USD_XR_diff$DP_5Y_logdiff)$y))

#install.packages("sfsmisc")
# require(sfsmisc)
#ecdf_native = ecdf(CDS1_USD_XR_diff$DP_5Y_diff)
#plot(ecdf_native(CDS1_USD_XR_diff$DP_5Y_diff),type="l")
#ecdf_native(CDS1_USD_XR_diff$DP_5Y_diff) == 1
# hist(ecdf_native(CDS1_USD_XR_diff$DP_5Y_diff), breaks=30)
# ecdf_density = empirical_cdf_density(CDS1_USD_XR_diff$DP_5Y_diff)
# sapply(CDS1_USD_XR_diff$DP_5Y_diff,ecdf_density)
# 
# par(mfrow=c(2,1))
# hist(ecdf_native(CDS1_USD_XR_diff$DP_5Y_diff), breaks=30, main="Distribution of U obtained using ecdf()",xlab="")
# hist(sapply(CDS1_USD_XR_diff$DP_5Y_diff,ecdf_density), breaks=30, main="Distribution of U obtained using density()",xlab="")
# 
# xxxx = seq(min(fn_density$x),max(fn_density$x),length=1000)

#----------------------------------------------------------------------------------
#Correlation Matrix for Gaussian copula (Log Difference of Default Probability - 5Y)
#----------------------------------------------------------------------------------

DP_5Y_logdiff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS2_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS3_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS4_USD_XR_diff$DP_5Y_logdiff),
  transform_to_normal(CDS5_USD_XR_diff$DP_5Y_logdiff)))

hist(transform_to_normal(CDS1_USD_XR_diff$DP_5Y_logdiff), breaks=30)

DP_5Y_logdiff_CorrelationMatrix_GaussianCopula
CorrelationMatrix_GaussianCopula = DP_5Y_logdiff_CorrelationMatrix_GaussianCopula


#----------------------------------------------------------------------------------
#Rolling 60days correlation (Log Difference of Default Probability - 5Y)
#----------------------------------------------------------------------------------
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

#par(mfrow=c(3,1))
#plot(seq(1,length(rolling_correlation_1vs5)),rolling_correlation_1vs5,type="l")
#plot(seq(1,length(rolling_correlation_1vs5)),rolling_correlation_3vs4,type="l")
#plot(seq(1,length(rolling_correlation_1vs5)),rolling_correlation_2vs3,type="l")
#matplot(seq(1,length(rolling_correlation_1vs5)),cbind(rolling_correlation_1vs5,rolling_correlation_3vs4,rolling_correlation_2vs3),type="l")
matplot(seq(1,length(rolling_correlation_1vs5)),cbind(rolling_correlation_1vs5,rolling_correlation_3vs4),type="l",col=c("black","blue"),lty=1,xlab="Time",ylab="Correlation",ylim=c(0,0.75))
legend(68.5, 0.14, c("Corr(BMY,PFE)","Corr(HP,IBM)"), col = c("black","blue"), text.col = "black", lty = c(1,1), pch = c(NA),
merge = TRUE, bg = "gray90")

#------------------------
#correlation stability
#------------------------

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

#par(mfrow=c(1,1))
#----------------------------------------------------------------------------------
#Correlation Matrix for Gaussian copula (Hazard Rate - 5Y)
#----------------------------------------------------------------------------------

HR_5Y_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS2_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS3_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS4_USD_XR_diff$HR_5Y),
  transform_to_normal(CDS5_USD_XR_diff$HR_5Y)))

HR_5Y_CorrelationMatrix_GaussianCopula


#-----------------------------------------------------------------------------
#Correlation Matrix for Gaussian copula (Difference of Hazard Rate - 5Y)
#-----------------------------------------------------------------------------

HR_5Y_diff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS2_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS3_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS4_USD_XR_diff$HR_5Y_diff),
  transform_to_normal(CDS5_USD_XR_diff$HR_5Y_diff)))

HR_5Y_diff_CorrelationMatrix_GaussianCopula

#-----------------------------------------------------------------------------
#Correlation Matrix for Gaussian copula (Log Difference of Hazard Rate - 5Y)
#-----------------------------------------------------------------------------

HR_5Y_logdiff_CorrelationMatrix_GaussianCopula = pearson_correlation_matrix(rbind(
  transform_to_normal(CDS1_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS2_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS3_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS4_USD_XR_diff$HR_5Y_logdiff),
  transform_to_normal(CDS5_USD_XR_diff$HR_5Y_logdiff)))

HR_5Y_logdiff_CorrelationMatrix_GaussianCopula

#----------------------------------------------------------------------------------
#Rolling 60days correlation (Log Difference of Hazard rates - 5Y)
#----------------------------------------------------------------------------------

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


#par(mfrow=c(1,3))
#plot(density(CDS1_USD_XR_diff$HR_5Y[!is.na(CDS1_USD_XR_diff$HR_5Y)]),main="Hazard rate",ylab="",xlab="",  ,col="black",lty=1)
#plot(density(CDS1_USD_XR_diff$HR_5Y_diff[!is.na(CDS1_USD_XR_diff$HR_5Y_diff)]),main="Hazard rate (diff)",ylab="",xlab="",col="black",lty=1)
#plot(density(CDS1_USD_XR_diff$HR_5Y_logdiff[!is.na(CDS1_USD_XR_diff$HR_5Y_logdiff)]),main="Hazard rate (log diff)",ylab="",xlab="",col="black",lty=1)

#plot(density(CDS1_USD_XR_diff$HR_5Y[!is.na(CDS1_USD_XR_diff$HR_5Y)]),main="Kernel density plot for difference and log difference",ylab="",xlab="",  xaxt='n', yaxt='n',col="black",lty=1)
#par(new=TRUE)
#plot(density(CDS1_USD_XR_diff$HR_5Y_diff[!is.na(CDS1_USD_XR_diff$HR_5Y_diff)]),main="Kernel density plot for difference and log difference",ylab="",xlab="",  xaxt='n', yaxt='n',col="black",lty=1)
#par(new=TRUE)
#plot(density(CDS1_USD_XR_diff$HR_5Y_logdiff[!is.na(CDS1_USD_XR_diff$HR_5Y_logdiff)]),main="",ylab="",xlab="",xaxt='n', yaxt='n',col="black",lty=2)
#par(new=FALSE)

#legend(0.02, 25, c("5Y Default Probability difference","5Y Default Probability log difference"), col = c("black"),
#       text.col = "black", lty = c(1,2), pch = c(NA),
#       merge = TRUE, bg = "gray90")

#linear measure (Pearson)
#cor(x,y,method = "pearson")



# transform_to_uniform_kerneldensity = function(X) {
#   #ecdf does not rely on kernel density
#   #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
#   fn = empirical_cdf_density(X) 
#   U = sapply(X,fn)
#   return(U)
# }
# U_CDS1_USD_XR_kerneldensity = transform_to_uniform_kerneldensity(CDS1_USD_XR_diff$DP_5Y_diff)
#U_CDS5_USD_XR_kerneldensity = transform_to_uniform_kerneldensity(CDS5_USD_XR_PDdiff$DP_5Y_change)
#hist(U_CDS1_USD_XR_kerneldensity, breaks = 30)
#hist(U_CDS5_USD_XR_kerneldensity, breaks = 30)



#export_to_mathematica = function(arr) {
#  ret = "{"
#  for (i in seq(1,length(arr)-1)) {
#    ret = paste(ret,sprintf("%.15f",arr[i]),",")
#  }
#  ret = paste(ret,sprintf("%.15f",arr[length(arr)]),"}")
#  return(ret)
#}

#export_to_mathematica(CDS1_USD_XR_PDdiff$DP_5Y_change)

#import_from_mathematica = function(str) {
#  tmp = strsplit(str, ",")[[1]]
#  tmp = gsub("\n", "", tmp)
#  tmp = as.numeric(tmp)
#  return(tmp)
#}



#using logspline package. not very uniform
#install.packages("logspline")
#require(logspline)
#U_CDS1_USD_XR_logspline = plogspline(CDS1_USD_XR_PDdiff$DP_5Y_change,logspline(CDS1_USD_XR_PDdiff$DP_5Y_change))
#U_CDS5_USD_XR_logspline = plogspline(CDS5_USD_XR_PDdiff$DP_5Y_change,logspline(CDS5_USD_XR_PDdiff$DP_5Y_change))
#U_CDS4_USD_XR_logspline = plogspline(CDS4_USD_XR_PDdiff$DP_5Y_change,logspline(CDS4_USD_XR_PDdiff$DP_5Y_change))
#U_CDS2_USD_XR_logspline = plogspline(CDS2_USD_XR_PDdiff$DP_5Y_change,logspline(CDS2_USD_XR_PDdiff$DP_5Y_change))
#U_CDS3_USD_XR_logspline = plogspline(CDS3_USD_XR_PDdiff$DP_5Y_change,logspline(CDS3_USD_XR_PDdiff$DP_5Y_change))
#hist(U_CDS1_USD_XR_logspline)
#hist(U_CDS5_USD_XR_logspline)
#hist(U_CDS4_USD_XR_logspline)
#hist(U_CDS2_USD_XR_logspline)
#hist(U_CDS3_USD_XR_logspline)


#kernel smoothing CDF using density() -- alternative to ecdf
#install.packages("sfsmisc")
#require(sfsmisc)
#empirical_cdf_density = function(data) {
#  density = density(data, n = 50)
#  func = function(x) {
#    if (x <= min(density$x)) {
#      return(0)
#    }
#    else {
#      integrate.xy(density$x,density$y,min(density$x),min(x,max(density$x)))
#    }      
#  }
#  return(func)
#}

#http://www.mathworks.com/help/stats/examples/nonparametric-estimates-of-cumulative-distribution-functions-and-their-inverses.html
#lulu = CDS4_USD_XR_PDdiff$DP_5Y_change
#The ecdf function computes one type of nonparametric CDF estimate, the empirical CDF, which is a stairstep function
#fn_ecdf = ecdf(lulu)
#plot(fn_ecdf)
#fn_empirical_cdf_density = empirical_cdf_density(lulu)

#plot_x_lim = c(-0.003,0.005);
#plot_y_lim = c(0,1);
#plot(fn_ecdf,col="blue",xlim=plot_x_lim, ylim=plot_y_lim)
#par(new=TRUE);
#xxxx = seq(min(fn_density$x),max(fn_density$x),length=1000)
#plot(xxxx,sapply(xxxx,fn_empirical_cdf_density),type="l",col="red",xlim=plot_x_lim, ylim=plot_y_lim)

#  empirical_cdf_density = function(data) {
#    density = density(data)
#    func = function(x) {
#      if (length(x) == 1) {
#        cat("single value\n")
#        if (x <= min(density$x)) {
#          return(0)
#        }
#        else {
#          return(integrate.xy(density$x,density$y,min(density$x),min(x,max(density$x))))
#        }     
#      }
#      else if (length(x) == 0) {
#        cat("error: input parameter of length zero\n")
#      }
#      else {
#        cat("multi value\n")
#        return(sapply(x,empirical_cdf_density))
#      }    
#    }
#    return(func)
#  }

#Correlation for Student t copula

#since data is close to normal, ecdf should give acceptable results
#explore alternative with mathematica / matlab
#mathematica test -- not really conclusive
transform_to_uniform = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
  fn = ecdf(X) 
  U = fn(X)
  return(U)
}


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


CorrelationMatrix_KendallTau = kendall_correlation_matrix(rbind(
  transform_to_uniform(CDS1_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS2_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS3_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS4_USD_XR_diff$DP_5Y_logdiff),
  transform_to_uniform(CDS5_USD_XR_diff$DP_5Y_logdiff)
  ))


#get good "uniform" result with ecdf
hist(transform_to_uniform(CDS1_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS2_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS3_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS4_USD_XR_diff$DP_5Y_logdiff),breaks = 30)
hist(transform_to_uniform(CDS5_USD_XR_diff$DP_5Y_logdiff),breaks = 30)


#infer linear correlation matrix
#convert kendall tau coeedficient to normal cooefficient
#correlation matrix to be fed into copula 
#(copula density fitting and sampling from copula procedure)
CorrelationMatrix_StudentTCopula = sin(0.5*pi*CorrelationMatrix_KendallTau)
rownames(CorrelationMatrix_StudentTCopula) = AssetTicker 
colnames(CorrelationMatrix_StudentTCopula) = AssetTicker 

#Correlation Experiment: estimate rank correlation for changes in
#credit spreads, survival probabilities, hazard rates. Rank measures
#are invariant to the choice of distribution bF(X).

#MLE for student's t copula

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

#test of C
#C(U_array[22,],2,DefaultProbabilityMatrix_StudentTCopula)

loglikelihoodfunc = function(UMatrix,v,Sigma) {
  value = 0
  for (i in seq(1,nrow(UMatrix))) {
    value = value + log(C(UMatrix[i,],v,Sigma))
    #cat("[i=",i,"] =>",value,"\n")
  }
  return(value)
}

#suppress observation where one or more ui = 1.00; these elements crash the MLE
U_array = cbind(transform_to_uniform(CDS1_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS2_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS3_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS4_USD_XR_diff$DP_5Y_logdiff),
                transform_to_uniform(CDS5_USD_XR_diff$DP_5Y_logdiff))


U_array = U_array[(U_array[,1]!=1.0 & U_array[,2]!=1.0 & U_array[,3]!=1.0 & U_array[,4]!=1.0 & U_array[,5]!=1.0) &
                  !is.na(U_array[,1]) & !is.na(U_array[,2]) & !is.na(U_array[,3]) & !is.na(U_array[,4]) & !is.na(U_array[,5]),]
#loglikelihoodfunc(U_array,10,CorrelationMatrix_StudentTCopula)

#plot of the log-likelihood function
v_array = seq(1,60,by=1)
loglikelihood_array = sapply(v_array,loglikelihoodfunc,UMatrix=U_array,Sigma=CorrelationMatrix_StudentTCopula)
plot(v_array,loglikelihood_array,type="l",xlab="Degrees of freedom",ylab="Log-likelyhood",log="y",ylim=c(105,115))

#optimization to compute degree of freedom for student t
degree_freedom = optimize(loglikelihoodfunc, UMatrix = U_array, Sigma = CorrelationMatrix_StudentTCopula, interval=c(1, 50),maximum=TRUE)$maximum

loglikelihoodfunc(U_array,0.1,CorrelationMatrix_StudentTCopula)

# 
# UBLN = U_array[22,]
# nBLN = length(UBLN)
# SigmaBLN = DefaultProbabilityMatrix_StudentTCopula
# vBLN = 10
# (1 + ((qt(t(UBLN), df=vBLN) %*% solve(SigmaBLN) %*% qt(UBLN, df=vBLN))/vBLN))^(-(vBLN+nBLN)/2)
# 
# val = 1
# for (i in seq(1,nBLN)) {
#   val = val * (1+((qt(UBLN[i],df=vBLN)^2)/vBLN))^(-(vBLN+1)/2) 
# } 
