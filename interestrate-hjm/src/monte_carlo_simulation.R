PC1_volatility_fitted = function(Tau) {
  return(0.0064306548)
}

PC2_volatility_fitted = function(Tau) {
  return(-0.0035565431 + Tau*(-0.0005683999) + Tau^2 * 0.0001181915 + Tau^3 * (-0.0000035939))
}

PC3_volatility_fitted = function(Tau) {
  return(-0.0047506715 + Tau * 0.0017541783 + Tau ^ 2 * (-0.0001415249) + Tau ^ 3 * 0.0000031274)
}

M = function(Tau) {
# This funciton carries out integration for all principal factors. 
# It uses the fact that volatility is function of time in HJM model
  
  if (Tau == 0) {
    return(0)
  }
  else {
    dTau = 0.01
    NumberOfSlice = floor(Tau/dTau)
    
    #M1 / M2 / M3
    M1 = integral_trapezium_rule(PC1_volatility_fitted,0,Tau,NumberOfSlice)
    M2 = integral_trapezium_rule(PC2_volatility_fitted,0,Tau,NumberOfSlice)
    M3 = integral_trapezium_rule(PC3_volatility_fitted,0,Tau,NumberOfSlice)
    
    return(PC1_volatility_fitted(Tau)*M1+PC2_volatility_fitted(Tau)*M2+PC3_volatility_fitted(Tau)*M3)
  }
}

input_data = read.csv("P:\\CQF\\FinalProject\\git-root\\finalproject\\interestrate-hjm\\data\\spot_curve.csv", header = TRUE)
#dX_data = read.csv("P:\\CQF\\Module4\\Lecture 5\\DX.csv", header = TRUE)
timestamp = Sys.time()

#NumberOfMaturity = nrow(input_data)
NumberOfMaturity = 5
maturityBucket = 0.5
NumberOfYear = 2 #projection of the forward rates evolation over 10 years
timestep = 0.01 #size of timestep for projection
NumberOfTimesteps = NumberOfYear/timestep
NumberSimulation = 10000
Result = rep(NA,NumberSimulation)
ConvergenceDiagram = matrix(NA, ncol=2, nrow=(NumberSimulation/100)+1)
colnames(ConvergenceDiagram) = c("nbsimul", "value")


#pre-calculation (performance)
Maturity = seq(0,25,by=maturityBucket)
drift = rep(NA,ncol(mat))
volatility_1 = rep(NA,ncol(mat))
volatility_2 = rep(NA,ncol(mat))
volatility_3 = rep(NA,ncol(mat))

for (j in seq(1,ncol(mat))) {
  drift[j] = M(Maturity[j])
  volatility_1[j] = PC1_volatility_fitted(Maturity[j])
  volatility_2[j] = PC2_volatility_fitted(Maturity[j])
  volatility_3[j] = PC3_volatility_fitted(Maturity[j])
}

#timestamp1 = Sys.time()
l=1
for (k in seq(1,NumberSimulation)) {
  #cat(k,"...\n")
  if (k%%(NumberSimulation/20) == 0) cat((k/NumberSimulation)*100,"% ...\n")
  
  mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=NumberOfMaturity,byrow = TRUE);
  #initialize first row (equivalent to t=0) with input data (latest spot curve)
  #mat[1,] = input_data$rate
  mat[1,] = input_data$rate[1:5]
  
  #timestamp2 = Sys.time()
  #print(timestamp2-timestamp1)
  #timestamp1 = timestamp2
  
  #set.seed(165468833)
  #require(fOptions)
  #ZMatrix_gaussian = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
  dX = rnorm.sobol(n = NumberSimulation+1, dimension = 3 , scrambling = 3)
  #dX = matrix(rnorm((NumberOfTimesteps+1)*3,mean = 0,sd =1),ncol=3,nrow=(NumberOfTimesteps+1),byrow = FALSE)
  
  #dX_1 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
  #dX_2 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
  #dX_3 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
  #dX_1 = dX_data$dX_1
  #dX_2 = dX_data$dX_2
  #dX_3 = dX_data$dX_3
  
  #i: timestep for projection
  #j: maturity of the curve
  
  #timestamp2 = Sys.time()
  #print(timestamp2-timestamp1)
  #timestamp1 = timestamp2
  
  #populate matrix
  for (i in seq(2,nrow(mat))) {
    #timestamp2 = Sys.time()
    #cat("i=",i,"==>",timestamp2-timestamp1,"\n")
    #timestamp1 = timestamp2
    
    for (j in seq(1,ncol(mat))) {
      #Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
      if (j<ncol(mat)) {
        mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
          sum(volatility_1[j]*dX[i,1],volatility_2[j]*dX[i,2],volatility_3[j]*dX[i,3])*sqrt(timestep) +
          ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
        
      }
      else if (j == ncol(mat)) {
        #use backward difference for dF/dTau on last column
        mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
          sum(volatility_1[j]*dX[i,1],volatility_2[j]*dX[i,2],volatility_3[j]*dX[i,3])*sqrt(timestep) +
          ((mat[i-1,j]-mat[i-1,j-1])/(maturityBucket))*timestep 
      }
      
    }
  }
  
  #timestamp2 = Sys.time()
  #print(timestamp2-timestamp1)
  #timestamp1 = timestamp2
  
  #calculate value
  #we sum the f(t) for maturity 0.08 (1M) which we can as approximation for r(t)
  #1:100 because 1Y bond
  Result[k] = exp(-1*sum(mat[1:100,1]*timestep))
  

  if (k %% 100 == 0 || k == 10) {
    ConvergenceDiagram[l,"nbsimul"] = k
    ConvergenceDiagram[l,"value"] = mean(Result[1:k])
    l = l+1
  }
}
BondPrice= mean(Result)
BondPrice
plot(ConvergenceDiagram[,"nbsimul"],ConvergenceDiagram[,"value"],type="l")
cat("Time to compute:", Sys.time()-timestamp,"\n")

#rbind(drift,volatility_1,volatility_2,volatility_3)












#plot_x_lim = c(0,25);
#plot_y_lim = c(0,0.08);
#plot(Maturity,mat[1,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",
#     main="Forward Rates Term Structure (Zero curve)",xlab="Maturity, year",ylab="")
#par(new=TRUE);
#plot(Maturity,mat[100,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="pink",
#     main="",xlab="",ylab="")
#par(new=TRUE);
#plot(Maturity,mat[500,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="yellow",
#     main="",xlab="",ylab="")
#par(new=TRUE);
#plot(Maturity,mat[1000,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="cyan",
#     main="",xlab="",ylab="")





