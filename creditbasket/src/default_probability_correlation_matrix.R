#arrays of CreditCurve Objects
BMY_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(BMY_USD_XR,HistoricalYieldCurveMatrix)
DELL_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(DELL_USD_XR,HistoricalYieldCurveMatrix)
HP_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(HP_USD_XR,HistoricalYieldCurveMatrix)
IBM_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(IBM_USD_XR,HistoricalYieldCurveMatrix)
PFE_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(PFE_USD_XR,HistoricalYieldCurveMatrix)

#dataframe with 5Y Survival Probability and Default Probability
BMY_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(BMY_USD_XR_HistCreditCurve)
DELL_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(DELL_USD_XR_HistCreditCurve)
HP_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(HP_USD_XR_HistCreditCurve)
IBM_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(IBM_USD_XR_HistCreditCurve)
PFE_USD_XR_HistCreditCurveDataframe = ConvertHistoricCreditCurveToDataframe(PFE_USD_XR_HistCreditCurve)

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

BMY_USD_XR_PDdiff = ComputeDifference(BMY_USD_XR_HistCreditCurveDataframe)
DELL_USD_XR_PDdiff = ComputeDifference(DELL_USD_XR_HistCreditCurveDataframe)
HP_USD_XR_PDdiff = ComputeDifference(HP_USD_XR_HistCreditCurveDataframe)
IBM_USD_XR_PDdiff = ComputeDifference(IBM_USD_XR_HistCreditCurveDataframe)
PFE_USD_XR_PDdiff = ComputeDifference(PFE_USD_XR_HistCreditCurveDataframe)

#represent plot
plot(BMY_USD_XR_PDdiff$Date,BMY_USD_XR_PDdiff$DP_5Y_change,type="l")

BMY_USD_XR_PDdiff$DP_5Y_change[32]

#represent histograms
hist(IBM_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(PFE_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(HP_USD_XR_PDdiff$DP_5Y_change, breaks=30)
hist(BMY_USD_XR_PDdiff$DP_5Y_change, breaks=30)

#correlation for gaussian copula
transform_pddiff_to_normal = function(X) {
  fn = ecdf(X) 
  U = fn(X)
  Z = qnorm(U)
  return(Z)
}

#must clean data earlier
Z_BMY_USD_XR = transform_pddiff_to_normal(BMY_USD_XR_PDdiff$DP_5Y_change)
Z_PFE_USD_XR = transform_pddiff_to_normal(PFE_USD_XR_PDdiff$DP_5Y_change)
Z_IBM_USD_XR = transform_pddiff_to_normal(IBM_USD_XR_PDdiff$DP_5Y_change)
Z_DELL_USD_XR = transform_pddiff_to_normal(DELL_USD_XR_PDdiff$DP_5Y_change)
Z_HP_USD_XR = transform_pddiff_to_normal(HP_USD_XR_PDdiff$DP_5Y_change)

hist(Z_BMY_USD_XR)

correlation_exclude_spikes = function(Z1,Z2) {
  #check for Inf
  spikes_for_Z1 = !(Z1 != Inf)
  spikes_for_Z2 = !(Z2 != Inf)
  spikes_for_both = spikes_for_Z1 | spikes_for_Z2  
  cat(sum(spikes_for_both == TRUE)," date(s) excluded in both time series...\n")
  
  return(cor(Z1[spikes_for_both==FALSE],Z2[spikes_for_both==FALSE], method = "pearson"))
}

myPDdiffMatrix = rbind(Z_BMY_USD_XR,Z_PFE_USD_XR,Z_IBM_USD_XR,Z_DELL_USD_XR,Z_HP_USD_XR)

#Correlation for Gaussian copula
DefaultProbabilityMatrix_GaussianCopula = matrix(NA, 
                           nrow=nrow(myPDdiffMatrix),
                           ncol=nrow(myPDdiffMatrix),
                           byrow = TRUE);

for (i in seq(1,nrow(myPDdiffMatrix))) {
  for (j in seq(1,nrow(myPDdiffMatrix))) {
    DefaultProbabilityMatrix_GaussianCopula[i,j] = correlation_exclude_spikes(myPDdiffMatrix[i,],myPDdiffMatrix[j,])
  }
}

cat("Z_BMY_USD_XR,Z_PFE_USD_XR,Z_IBM_USD_XR,Z_DELL_USD_XR,Z_HP_USD_XR\n")
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


U_BMY_USD_XR = transform_pddiff_to_uniform(BMY_USD_XR_PDdiff$DP_5Y_change)
U_PFE_USD_XR = transform_pddiff_to_uniform(PFE_USD_XR_PDdiff$DP_5Y_change)
U_IBM_USD_XR = transform_pddiff_to_uniform(IBM_USD_XR_PDdiff$DP_5Y_change)
U_DELL_USD_XR = transform_pddiff_to_uniform(DELL_USD_XR_PDdiff$DP_5Y_change)
U_HP_USD_XR = transform_pddiff_to_uniform(HP_USD_XR_PDdiff$DP_5Y_change)


#transform_pddiff_to_uniform_kerneldensity = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
#  fn = empirical_cdf_density(X) 
#  U = sapply(X,fn)
#  return(U)
#}
#U_BMY_USD_XR_kerneldensity = transform_pddiff_to_uniform_kerneldensity(BMY_USD_XR_PDdiff$DP_5Y_change)
#U_PFE_USD_XR_kerneldensity = transform_pddiff_to_uniform_kerneldensity(PFE_USD_XR_PDdiff$DP_5Y_change)
#hist(U_BMY_USD_XR_kerneldensity, breaks = 30)
#hist(U_PFE_USD_XR_kerneldensity, breaks = 30)



#export_to_mathematica = function(arr) {
#  ret = "{"
#  for (i in seq(1,length(arr)-1)) {
#    ret = paste(ret,sprintf("%.15f",arr[i]),",")
#  }
#  ret = paste(ret,sprintf("%.15f",arr[length(arr)]),"}")
#  return(ret)
#}

#export_to_mathematica(BMY_USD_XR_PDdiff$DP_5Y_change)

#import_from_mathematica = function(str) {
#  tmp = strsplit(str, ",")[[1]]
#  tmp = gsub("\n", "", tmp)
#  tmp = as.numeric(tmp)
#  return(tmp)
#}



#using logspline package. not very uniform
#install.packages("logspline")
#require(logspline)
#U_BMY_USD_XR_logspline = plogspline(BMY_USD_XR_PDdiff$DP_5Y_change,logspline(BMY_USD_XR_PDdiff$DP_5Y_change))
#U_PFE_USD_XR_logspline = plogspline(PFE_USD_XR_PDdiff$DP_5Y_change,logspline(PFE_USD_XR_PDdiff$DP_5Y_change))
#U_IBM_USD_XR_logspline = plogspline(IBM_USD_XR_PDdiff$DP_5Y_change,logspline(IBM_USD_XR_PDdiff$DP_5Y_change))
#U_DELL_USD_XR_logspline = plogspline(DELL_USD_XR_PDdiff$DP_5Y_change,logspline(DELL_USD_XR_PDdiff$DP_5Y_change))
#U_HP_USD_XR_logspline = plogspline(HP_USD_XR_PDdiff$DP_5Y_change,logspline(HP_USD_XR_PDdiff$DP_5Y_change))
#hist(U_BMY_USD_XR_logspline)
#hist(U_PFE_USD_XR_logspline)
#hist(U_IBM_USD_XR_logspline)
#hist(U_DELL_USD_XR_logspline)
#hist(U_HP_USD_XR_logspline)


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
#lulu = IBM_USD_XR_PDdiff$DP_5Y_change
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
hist(U_BMY_USD_XR,breaks = 30)
hist(U_PFE_USD_XR,breaks = 30)
hist(U_IBM_USD_XR,breaks = 30)
hist(U_DELL_USD_XR,breaks = 30)
hist(U_HP_USD_XR, breaks = 30)

myUnifMatrix = rbind(U_BMY_USD_XR,U_PFE_USD_XR,U_IBM_USD_XR,U_DELL_USD_XR,U_HP_USD_XR)

DefaultProbabilityMatrix_KendallTau = matrix(NA, 
                                nrow=nrow(myUnifMatrix),
                                ncol=nrow(myUnifMatrix),
                                byrow = TRUE);

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

#Correlation Experiment: estimate rank correlation for changes in
#credit spreads, survival probabilities, hazard rates. Rank measures
#are invariant to the choice of distribution bF(X).

#MLE for student's t copula

myU = cbind(U_BMY_USD_XR,U_PFE_USD_XR,U_IBM_USD_XR,U_DELL_USD_XR,U_HP_USD_XR)

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

#suppress row number 33, all ui = 1 
loglikelyhoodfunc(myU[-33,],10,DefaultProbabilityMatrix_StudentTCopula)

#plot of the log-lekelyhood funciton
v_array = seq(1,50)
loglikelyhood_array = sapply(v_array,loglikelyhoodfunc,UMatrix=myU[-33,],Sigma=DefaultProbabilityMatrix_StudentTCopula)
plot(v_array,loglikelyhood_array,type="l")

#optimization to compute degree of freedom for student t
degree_freedom = optimize(loglikelyhoodfunc, UMatrix = myU[-33,], Sigma = DefaultProbabilityMatrix_StudentTCopula, interval=c(1, 50),maximum=TRUE)$maximum

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
