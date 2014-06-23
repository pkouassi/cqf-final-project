#parallel computing set-up
cl = makeCluster(detectCores())
registerDoParallel(cl)
cat("Number of core(s) to be used for calculation:",getDoParWorkers(),"\n")
#cl_1core = makeCluster(1)
#registerDoParallel(cl_1core)


input_data = ValuationDateForwardCurve[1,]/100
#input_data = read.csv("P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/data/spot_curve.csv", header = TRUE)$rate
#dX_data = read.csv("P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/data/DX.csv", header = TRUE)
#dX_data = read.delim("clipboard")

timestamp = Sys.time()

#NumberOfMaturity = nrow(input_data)
MaxMaturity = 5
maturityBucket = 1/12
NumberOfYear = 2 #projection of the forward rates evolation over 10 years
timestep = 0.01 #size of timestep for projection
NumberOfTimesteps = NumberOfYear/timestep
NumberSimulation = 1000

#pre-calculation (performance)
MaturityList = c(0,seq(2/12,MaxMaturity,by=maturityBucket)) #1M rate is taken as proxy for Maturity=0
drift = rep(NA,length(MaturityList))
volatility_1 = rep(NA,length(MaturityList))
volatility_2 = rep(NA,length(MaturityList))
volatility_3 = rep(NA,length(MaturityList))

for (j in seq(1,length(MaturityList))) {
  drift[j] = M(MaturityList[j])
  volatility_1[j] = PC1_volatility_fitted(MaturityList[j])
  volatility_2[j] = PC2_volatility_fitted(MaturityList[j])
  volatility_3[j] = PC3_volatility_fitted(MaturityList[j])
}

dX_Sobol = rnorm.sobol(n = NumberSimulation, dimension = 3*NumberOfTimesteps , scrambling = 3)
Result = foreach(k=1:NumberSimulation, .combine=rbind) %dopar% {
#for (k in seq(1,NumberSimulation)) {
  #cat(k,"...\n")
  if (k%%(NumberSimulation/20) == 0) cat((k/NumberSimulation)*100,"% ...\n")
  
  #mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
  mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=24,byrow = TRUE);
  #initialize first row (equivalent to t=0) with input data (latest spot curve)
  #mat[1,] = input_data$rate
  
  mat[1,] = input_data[1:24]
  #mat[1,] = input_data[1:length(MaturityList)]

  dX = matrix(dX_Sobol[k,],ncol=3,nrow=NumberOfTimesteps,byrow = TRUE)
  #dX = dX_data
  #dX = matrix(rnorm(NumberOfTimesteps*3,mean = 0,sd =1),ncol=3,nrow=NumberOfTimesteps,byrow = FALSE)
  
  #dX_1 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
  #dX_2 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
  #dX_3 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
  #dX_1 = dX_data$dX_1
  #dX_2 = dX_data$dX_2
  #dX_3 = dX_data$dX_3
  
  #i: timestep for projection
  #j: maturity of the curve
  
  #populate matrix
  for (i in seq(2,nrow(mat))) {
    
    for (j in seq(1,ncol(mat))) {
      #Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
      if (j<ncol(mat)) {
        mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
          sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
          ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
        
      }
      else if (j == ncol(mat)) {
        #use backward difference for dF/dTau on last column
        mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
          sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
          ((mat[i-1,j]-mat[i-1,j-1])/(maturityBucket))*timestep 
      }
      
    }
  }
  
  #calculate value of a 1Y bond
  #we sum the f(t) for maturity 0.08 (1M) which we can as approximation for r(t)
  #1:100 because 1Y bond
  #Result[k] = exp(-1*sum(mat[1:100,1]*timestep))
  bond1Y = exp(-1*sum((mat[1:101,1])*timestep))
  bond2Y = exp(-1*sum((mat[1:201,1])*timestep))
  
  #calculate value of Caplet with strike k(1year forward starting 3M duration)
  
  x1 = seq(1/12,2,by=1/12)
  x2 = x1^2
  x3 = x2^3
  y = mat[101,]
  fit = lm(y~x1+x2+x3)
  b0 = as.numeric(fit$coefficients["(Intercept)"])
  b1 = as.numeric(fit$coefficients["x1"])
  b2 = as.numeric(fit$coefficients["x2"])
  b3 = as.numeric(fit$coefficients["x3"])
  tmpfunc = function(x) return(b0+b1*x+b2*x^2+b3*x^3)
  libor_continusously_coupounded = integrate(func,0,1)$value
  libor_simply_coupounded = exp(libor_continusously_coupounded)-1
  K = 0.008 #0.8%
  caplet1x1_008 = max(libor_simply_coupounded-0.008,0)*bond1Y*1
  caplet1x1_005 = max(libor_simply_coupounded-0.005,0)*bond1Y*1
  caplet1x1_002 = max(libor_simply_coupounded-0.002,0)*bond1Y*1
  
  res = c(bond1Y,caplet1x1_008,caplet1x1_005,caplet1x1_002)
  
  #if (k %% 100 == 0 || k == 10) {
  #  ConvergenceDiagram[l,"nbsimul"] = k
  #  ConvergenceDiagram[l,"value"] = mean(Result[1:k])
  #  l = l+1
  #}

}

stopCluster(cl)
#stopImplicitCluster()


BondPrice= mean(Result[,1])
cat("1Y Bond Price:",BondPrice,"\n")
cat("1x1 Caplet with K = 0.008 Price:",mean(Result[,2]),"\n")
cat("1x1 Caplet with K = 0.005 Price:",mean(Result[,3]),"\n")
cat("1x1 Caplet with K = 0.002 Price:",mean(Result[,4]),"\n")
cat("Time to compute:", Sys.time()-timestamp,"\n")

l=1
nbobservation = 100
ConvergenceDiagram = matrix(NA, ncol=2, nrow=(NumberSimulation/nbobservation)+1)
colnames(ConvergenceDiagram) = c("nbsimul", "value")
for (i in seq(1,NumberSimulation)) {
  if (i %% nbobservation == 0 || i == 10) {
    ConvergenceDiagram[l,"nbsimul"] = i
    ConvergenceDiagram[l,"value"] = mean(Result[1:i])
    l = l+1
  }
}

plot(ConvergenceDiagram[,"nbsimul"],ConvergenceDiagram[,"value"],type="l")







