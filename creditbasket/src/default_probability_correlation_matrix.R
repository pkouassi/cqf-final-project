#gather historical credit curve data
BMY_USD_XR=parseData("C://temp//markit","BMY","USD","XR")
DELL_USD_XR=parseData("C://temp//markit","DELLN","USD","XR")
HP_USD_XR=parseData("C://temp//markit","HPQ","USD","XR")
IBM_USD_XR=parseData("C://temp//markit","IBM","USD","XR")
PFE_USD_XR=parseData("C://temp//markit","PFE","USD","XR")

#only keep data from Monday 6-May-2013 to Friday 23-May-2014
BMY_USD_XR = BMY_USD_XR[BMY_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & BMY_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
DELL_USD_XR = DELL_USD_XR[DELL_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & DELL_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
HP_USD_XR = HP_USD_XR[HP_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & HP_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
IBM_USD_XR = IBM_USD_XR[IBM_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & IBM_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
PFE_USD_XR = PFE_USD_XR[PFE_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & PFE_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]

cbind(BMY_USD_XR$Date,DELL_USD_XR$Date,HP_USD_XR$Date,IBM_USD_XR$Date,PFE_USD_XR$Date)

#check that dates are all aligned
(BMY_USD_XR$Date == DELL_USD_XR$Date)
(BMY_USD_XR$Date == HP_USD_XR$Date)
(BMY_USD_XR$Date == IBM_USD_XR$Date)
(BMY_USD_XR$Date == PFE_USD_XR$Date)


#gather historical yield curve

#bootstrap credit curve for 1y data

#IBM / first date

BootstrapHistoricCreditCurve = function(HistCDSData) {
  RecoveryRate = 0.40
  HistCreditCurve = as.data.frame(matrix(ncol=4, nrow=nrow(HistCDSData)))
  names(HistCreditCurve) = c("Date", "Ticker","SP_5Y", "DP_5Y")
  
  for (i in seq(1,nrow(HistCDSData)))
  {
    CDS1Y = new ("CreditDefaultSwap", maturity = 1, marketprice = HistCDSData[i,]$Spread1y*10000)
    CDS2Y = new ("CreditDefaultSwap", maturity = 2, marketprice = HistCDSData[i,]$Spread2y*10000)
    CDS3Y = new ("CreditDefaultSwap", maturity = 3, marketprice = HistCDSData[i,]$Spread3y*10000)
    CDS4Y = new ("CreditDefaultSwap", maturity = 4, marketprice = HistCDSData[i,]$Spread4y*10000)
    CDS5Y = new ("CreditDefaultSwap", maturity = 5, marketprice = HistCDSData[i,]$Spread5y*10000)
    CDScol=c(CDS1Y,CDS2Y,CDS3Y,CDS4Y,CDS5Y)
    YieldCurve = getYieldCurve(HistYieldCurveMatrix,HistCDSData[i,]$Date)
    tmp = BootstrapCreditCurve(CDScol,RecoveryRate,YieldCurve)
    
    HistCreditCurve$Date[i] = HistCDSData[i,]$Date
    HistCreditCurve$Ticker[i] = HistCDSData[i,]$Ticker
    HistCreditCurve$SP_5Y[i] = tmp@survivalprobability[5]
    HistCreditCurve$DP_5Y[i] = 1 - HistCreditCurve$SP_5Y[i]
  }
  
  return(HistCreditCurve)
}

BMY_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(BMY_USD_XR)
DELL_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(DELL_USD_XR)
HP_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(HP_USD_XR)
IBM_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(IBM_USD_XR)
PFE_USD_XR_HistCreditCurve = BootstrapHistoricCreditCurve(PFE_USD_XR)

#calculate default probability difference
ComputeDifference = function(HistCreditCurve) {
  HistDifference = as.data.frame(matrix(ncol=3, nrow=(nrow(HistCreditCurve)-1)))
  names(HistDifference) = c("Date", "Ticker","DP_5Y_change")
  
  for (i in seq(2,nrow(HistCreditCurve))) {
    HistDifference$Date[i-1] = HistCreditCurve$Date[i]       
    HistDifference$Ticker[i-1] = HistCreditCurve$Ticker[i]
    #consider log difference (i.e. similar to hazard rate)
    HistDifference$DP_5Y_change[i-1] = (HistCreditCurve$DP_5Y[i] - HistCreditCurve$DP_5Y[i-1])    
  }
  return(HistDifference)
}

BMY_USD_XR_PDdiff = ComputeDifference(BMY_USD_XR_HistCreditCurve)
DELL_USD_XR_PDdiff = ComputeDifference(DELL_USD_XR_HistCreditCurve)
HP_USD_XR_PDdiff = ComputeDifference(HP_USD_XR_HistCreditCurve)
IBM_USD_XR_PDdiff = ComputeDifference(IBM_USD_XR_HistCreditCurve)
PFE_USD_XR_PDdiff = ComputeDifference(PFE_USD_XR_HistCreditCurve)

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
DefaultProbabilityMatrix = matrix(NA, 
                           nrow=nrow(myPDdiffMatrix),
                           ncol=nrow(myPDdiffMatrix),
                           byrow = TRUE);

for (i in seq(1,nrow(myPDdiffMatrix))) {
  for (j in seq(1,nrow(myPDdiffMatrix))) {
    DefaultProbabilityMatrix[i,j] = correlation_exclude_spikes(myPDdiffMatrix[i,],myPDdiffMatrix[j,])
  }
}

cat("Z_BMY_USD_XR,Z_PFE_USD_XR,Z_IBM_USD_XR,Z_DELL_USD_XR,Z_HP_USD_XR\n")
DefaultProbabilityMatrix

#linear measure (Pearson)
#cor(x,y,method = "pearson")

#Correlation for Student t copula

#since data is close to normal, ecdf should give acceptable results
transform_pddiff_to_uniform = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
  fn = ecdf(X) 
  U = fn(X)
  return(U)
}

transform_pddiff_to_uniform_kerneldensity = function(X) {
  #ecdf does not rely on kernel density
  #ecdf (Empirical CDF) in R instead of kernel densities. It summarizes the data into something like a smooth CDF line while graphing all the data points
  fn = empirical_cdf_density(X) 
  U = sapply(X,fn)
  return(U)
}

U_BMY_USD_XR = transform_pddiff_to_uniform(BMY_USD_XR_PDdiff$DP_5Y_change)
U_PFE_USD_XR = transform_pddiff_to_uniform(PFE_USD_XR_PDdiff$DP_5Y_change)
U_IBM_USD_XR = transform_pddiff_to_uniform(IBM_USD_XR_PDdiff$DP_5Y_change)
U_DELL_USD_XR = transform_pddiff_to_uniform(DELL_USD_XR_PDdiff$DP_5Y_change)
U_HP_USD_XR = transform_pddiff_to_uniform(HP_USD_XR_PDdiff$DP_5Y_change)

U_BMY_USD_XR_kerneldensity = transform_pddiff_to_uniform_kerneldensity(BMY_USD_XR_PDdiff$DP_5Y_change)
U_PFE_USD_XR_kerneldensity = transform_pddiff_to_uniform_kerneldensity(PFE_USD_XR_PDdiff$DP_5Y_change)

#using logspline package. not very uniform
#install.packages("logspline")
require(logspline)
U_BMY_USD_XR_logspline = plogspline(BMY_USD_XR_PDdiff$DP_5Y_change,logspline(BMY_USD_XR_PDdiff$DP_5Y_change))
U_PFE_USD_XR_logspline = plogspline(PFE_USD_XR_PDdiff$DP_5Y_change,logspline(PFE_USD_XR_PDdiff$DP_5Y_change))
U_IBM_USD_XR_logspline = plogspline(IBM_USD_XR_PDdiff$DP_5Y_change,logspline(IBM_USD_XR_PDdiff$DP_5Y_change))
U_DELL_USD_XR_logspline = plogspline(DELL_USD_XR_PDdiff$DP_5Y_change,logspline(DELL_USD_XR_PDdiff$DP_5Y_change))
U_HP_USD_XR_logspline = plogspline(HP_USD_XR_PDdiff$DP_5Y_change,logspline(HP_USD_XR_PDdiff$DP_5Y_change))


hist(U_BMY_USD_XR_logspline)
hist(U_PFE_USD_XR_logspline)
hist(U_IBM_USD_XR_logspline)
hist(U_DELL_USD_XR_logspline)
hist(U_HP_USD_XR_logspline)


#kernel smoothing CDF using density() -- alternative to ecdf
#install.packages("sfsmisc")
require(sfsmisc)
empirical_cdf_density = function(data) {
  density = density(data, n = 50)
  func = function(x) {
    if (x <= min(density$x)) {
      return(0)
    }
    else {
      integrate.xy(density$x,density$y,min(density$x),min(x,max(density$x)))
    }      
  }
  return(func)
}

#http://www.mathworks.com/help/stats/examples/nonparametric-estimates-of-cumulative-distribution-functions-and-their-inverses.html
lulu = IBM_USD_XR_PDdiff$DP_5Y_change
#The ecdf function computes one type of nonparametric CDF estimate, the empirical CDF, which is a stairstep function
fn_ecdf = ecdf(lulu)
plot(fn_ecdf)
fn_empirical_cdf_density = empirical_cdf_density(lulu)

plot_x_lim = c(-0.003,0.005);
plot_y_lim = c(0,1);
plot(fn_ecdf,col="blue",xlim=plot_x_lim, ylim=plot_y_lim)
par(new=TRUE);
xxxx = seq(min(fn_density$x),max(fn_density$x),length=1000)
plot(xxxx,sapply(xxxx,fn_empirical_cdf_density),type="l",col="red",xlim=plot_x_lim, ylim=plot_y_lim)

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
hist(U_BMY_USD_XR)
hist(U_PFE_USD_XR)
hist(U_IBM_USD_XR)
hist(U_DELL_USD_XR)
hist(U_HP_USD_XR, breaks = 20)

hist(U_BMY_USD_XR_kerneldensity, breaks = 30)
hist(U_PFE_USD_XR_kerneldensity, breaks = 30)






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
rho_matrix = sin(0.5*pi*DefaultProbabilityMatrix_KendallTau)

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
C(myU[22,],2,rho_matrix)

loglikelyhoodfunc = function(UMatrix,v,Sigma) {
  value = 0
  for (i in seq(1,nrow(UMatrix))) {
    value = value + log(C(UMatrix[i,],v,Sigma))
    cat("[i=",i,"] =>",value,"\n")
  }
  return(value)
}

#suppress row number 33, all ui = 1 
loglikelyhoodfunc(myU[-33,],10,rho_matrix)

#plot of the log-lekelyhood funciton
v_array = seq(1,50)
loglikelyhood_array = sapply(v_array,loglikelyhoodfunc,UMatrix=myU[-33,],Sigma=rho_matrix)
plot(v_array,loglikelyhood_array,type="l")

#optimization to compute degree of freedom for student t
degree_freedom = optimize(loglikelyhoodfunc, UMatrix = myU[-33,], Sigma = rho_matrix, interval=c(1, 50),maximum=TRUE)$maximum


# 
# UBLN = myU[22,]
# nBLN = length(UBLN)
# SigmaBLN = rho_matrix
# vBLN = 10
# (1 + ((qt(t(UBLN), df=vBLN) %*% solve(SigmaBLN) %*% qt(UBLN, df=vBLN))/vBLN))^(-(vBLN+nBLN)/2)
# 
# val = 1
# for (i in seq(1,nBLN)) {
#   val = val * (1+((qt(UBLN[i],df=vBLN)^2)/vBLN))^(-(vBLN+1)/2) 
# } 
