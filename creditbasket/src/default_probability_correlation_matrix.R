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
  HistDifference = as.data.frame(matrix(ncol=3, nrow=(nrow(HistCreditCurveDataframe)-1)))
  names(HistDifference) = c("Date", "Ticker","DP_5Y_change")
  
  for (i in seq(2,nrow(HistCreditCurveDataframe))) {
    HistDifference$Date[i-1] = HistCreditCurveDataframe$Date[i]       
    HistDifference$Ticker[i-1] = HistCreditCurveDataframe$Ticker[i]
    #consider log difference (i.e. similar to hazard rate)
    HistDifference$DP_5Y_change[i-1] = (HistCreditCurveDataframe$DP_5Y[i] - HistCreditCurveDataframe$DP_5Y[i-1])    
  }
  return(HistDifference)
}

CDS1_USD_XR_PDdiff = ComputeDifference(CDS1_USD_XR_HistCreditCurveDataframe)
CDS2_USD_XR_PDdiff = ComputeDifference(CDS2_USD_XR_HistCreditCurveDataframe)
CDS3_USD_XR_PDdiff = ComputeDifference(CDS3_USD_XR_HistCreditCurveDataframe)
CDS4_USD_XR_PDdiff = ComputeDifference(CDS4_USD_XR_HistCreditCurveDataframe)
CDS5_USD_XR_PDdiff = ComputeDifference(CDS5_USD_XR_HistCreditCurveDataframe)

#represent plot
plot(CDS1_USD_XR_PDdiff$Date,CDS1_USD_XR_PDdiff$DP_5Y_change,type="l")

CDS1_USD_XR_PDdiff$DP_5Y_change[33]

#represent histograms
hist(CDS4_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(CDS5_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(CDS3_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(CDS2_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(CDS1_USD_XR_PDdiff$DP_5Y_change, breaks=30)

cbind(CDS1_USD_XR_PDdiff$DP_5Y_change,CDS2_USD_XR_PDdiff$DP_5Y_change,CDS3_USD_XR_PDdiff$DP_5Y_change,CDS4_USD_XR_PDdiff$DP_5Y_change,CDS5_USD_XR_PDdiff$DP_5Y_change)

#correlation for gaussian copula
transform_pddiff_to_normal = function(X) {
  fn = ecdf(X) 
  U = fn(X)
  Z = qnorm(U)
  return(Z)
}

#must clean data earlier
Z_CDS1_USD_XR = transform_pddiff_to_normal(CDS1_USD_XR_PDdiff$DP_5Y_change)
Z_CDS2_USD_XR = transform_pddiff_to_normal(CDS2_USD_XR_PDdiff$DP_5Y_change)
Z_CDS3_USD_XR = transform_pddiff_to_normal(CDS3_USD_XR_PDdiff$DP_5Y_change)
Z_CDS4_USD_XR = transform_pddiff_to_normal(CDS4_USD_XR_PDdiff$DP_5Y_change)
Z_CDS5_USD_XR = transform_pddiff_to_normal(CDS5_USD_XR_PDdiff$DP_5Y_change)

hist(Z_CDS1_USD_XR, breaks=30)
hist(Z_CDS2_USD_XR, breaks=30)


correlation_exclude_spikes = function(Z1,Z2) {
  #check for Inf
  spikes_for_Z1 = !(Z1 != Inf)
  spikes_for_Z2 = !(Z2 != Inf)
  spikes_for_both = spikes_for_Z1 | spikes_for_Z2  
  cat(sum(spikes_for_both == TRUE)," date(s) excluded in both time series...\n")
  
  return(cor(Z1[spikes_for_both==FALSE],Z2[spikes_for_both==FALSE], method = "pearson"))
}

myPDdiffMatrix = rbind(Z_CDS1_USD_XR,Z_CDS2_USD_XR,Z_CDS3_USD_XR,Z_CDS4_USD_XR,Z_CDS5_USD_XR)

#Correlation for Gaussian copula
DefaultProbabilityMatrix_GaussianCopula = matrix(NA, 
                           nrow=nrow(myPDdiffMatrix),
                           ncol=nrow(myPDdiffMatrix),
                           byrow = TRUE);
rownames(DefaultProbabilityMatrix_GaussianCopula) = AssetTicker 
colnames(DefaultProbabilityMatrix_GaussianCopula) = AssetTicker 

for (i in seq(1,nrow(myPDdiffMatrix))) {
  for (j in seq(1,nrow(myPDdiffMatrix))) {
    DefaultProbabilityMatrix_GaussianCopula[i,j] = correlation_exclude_spikes(myPDdiffMatrix[i,],myPDdiffMatrix[j,])
  }
}

cat("Z_CDS1_USD_XR,Z_CDS5_USD_XR,Z_CDS4_USD_XR,Z_CDS2_USD_XR,Z_CDS3_USD_XR\n")
DefaultProbabilityMatrix_GaussianCopula

#linear measure (Pearson)
#cor(x,y,method = "pearson")

#Correlation for Student t copula

#since data is close to normal, ecdf should give acceptable results
#explore alternative with mathematica / matlab
#mathematica test -- not really conclusive
transform_pddiff_to_uniform = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
  fn = ecdf(X) 
  U = fn(X)
  return(U)
}


U_CDS1_USD_XR = transform_pddiff_to_uniform(CDS1_USD_XR_PDdiff$DP_5Y_change)
U_CDS5_USD_XR = transform_pddiff_to_uniform(CDS5_USD_XR_PDdiff$DP_5Y_change)
U_CDS4_USD_XR = transform_pddiff_to_uniform(CDS4_USD_XR_PDdiff$DP_5Y_change)
U_CDS2_USD_XR = transform_pddiff_to_uniform(CDS2_USD_XR_PDdiff$DP_5Y_change)
U_CDS3_USD_XR = transform_pddiff_to_uniform(CDS3_USD_XR_PDdiff$DP_5Y_change)


#transform_pddiff_to_uniform_kerneldensity = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
#  fn = empirical_cdf_density(X) 
#  U = sapply(X,fn)
#  return(U)
#}
#U_CDS1_USD_XR_kerneldensity = transform_pddiff_to_uniform_kerneldensity(CDS1_USD_XR_PDdiff$DP_5Y_change)
#U_CDS5_USD_XR_kerneldensity = transform_pddiff_to_uniform_kerneldensity(CDS5_USD_XR_PDdiff$DP_5Y_change)
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

# empirical_cdf_density = function(data) {
#   density = density(data)
#   func = function(x) {
#     if (length(x) == 1) {
#       cat("sigle value\n")
#       if (x <= min(density$x)) {
#         return(0)
#       }
#       else {
#         return(integrate.xy(density$x,density$y,min(density$x),min(x,max(density$x))))
#       }     
#     }
#     else if (length(x) == 0) {
#       cat("error: input parameter of length zero\n")
#     }
#     else {
#       cat("multi value\n")
#       return(sapply(x,empirical_cdf_density))
#     }    
#   }
#   return(func)
# }

#get good "uniform" result with ecdf
hist(U_CDS1_USD_XR,breaks = 30)
hist(U_CDS5_USD_XR,breaks = 30)
hist(U_CDS4_USD_XR,breaks = 30)
hist(U_CDS2_USD_XR,breaks = 30)
hist(U_CDS3_USD_XR, breaks = 30)

myUnifMatrix = rbind(U_CDS1_USD_XR,U_CDS2_USD_XR,U_CDS3_USD_XR,U_CDS4_USD_XR,U_CDS5_USD_XR)

DefaultProbabilityMatrix_KendallTau = matrix(NA, 
                                nrow=nrow(myUnifMatrix),
                                ncol=nrow(myUnifMatrix),
                                byrow = TRUE);
rownames(DefaultProbabilityMatrix_KendallTau) = AssetTicker 
colnames(DefaultProbabilityMatrix_KendallTau) = AssetTicker 

for (i in seq(1,nrow(myUnifMatrix))) {
  for (j in seq(1,nrow(myUnifMatrix))) {
    DefaultProbabilityMatrix_KendallTau[i,j] = cor(myUnifMatrix[i,],myUnifMatrix[j,], method = "kendall")
  }
}

#infer linear correlation matrix
#convert kendall tau coeedficient to normal cooefficient
#correlation matrix to be fed into copula 
#(copula density fitting and sampling from copula procedure)
DefaultProbabilityMatrix_StudentTCopula = sin(0.5*pi*DefaultProbabilityMatrix_KendallTau)
rownames(DefaultProbabilityMatrix_StudentTCopula) = AssetTicker 
colnames(DefaultProbabilityMatrix_StudentTCopula) = AssetTicker 

#Correlation Experiment: estimate rank correlation for changes in
#credit spreads, survival probabilities, hazard rates. Rank measures
#are invariant to the choice of distribution bF(X).

#MLE for student's t copula

myU = cbind(U_CDS1_USD_XR,U_CDS5_USD_XR,U_CDS4_USD_XR,U_CDS2_USD_XR,U_CDS3_USD_XR)

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
#C(myU[22,],2,DefaultProbabilityMatrix_StudentTCopula)

loglikelyhoodfunc = function(UMatrix,v,Sigma) {
  value = 0
  for (i in seq(1,nrow(UMatrix))) {
    value = value + log(C(UMatrix[i,],v,Sigma))
    #cat("[i=",i,"] =>",value,"\n")
  }
  return(value)
}

#suppress observation where one or more ui = 1.00; these elements crash the MLE
myU = myU[(myU[,1]!=1.0 & myU[,2]!=1.0 & myU[,3]!=1.0 & myU[,4]!=1.0 & myU[,5]!=1.0),]
loglikelyhoodfunc(myU,10,DefaultProbabilityMatrix_StudentTCopula)

#plot of the log-lekelyhood funciton
v_array = seq(1,50)
loglikelyhood_array = sapply(v_array,loglikelyhoodfunc,UMatrix=myU,Sigma=DefaultProbabilityMatrix_StudentTCopula)
plot(v_array,loglikelyhood_array,type="l")

#optimization to compute degree of freedom for student t
degree_freedom = optimize(loglikelyhoodfunc, UMatrix = myU, Sigma = DefaultProbabilityMatrix_StudentTCopula, interval=c(1, 50),maximum=TRUE)$maximum

# 
# UBLN = myU[22,]
# nBLN = length(UBLN)
# SigmaBLN = DefaultProbabilityMatrix_StudentTCopula
# vBLN = 10
# (1 + ((qt(t(UBLN), df=vBLN) %*% solve(SigmaBLN) %*% qt(UBLN, df=vBLN))/vBLN))^(-(vBLN+nBLN)/2)
# 
# val = 1
# for (i in seq(1,nBLN)) {
#   val = val * (1+((qt(UBLN[i],df=vBLN)^2)/vBLN))^(-(vBLN+1)/2) 
# } 
