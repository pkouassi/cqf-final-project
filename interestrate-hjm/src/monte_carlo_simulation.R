#parallel computing set-up
cl = makeCluster(detectCores())
registerDoParallel(cl)
cat("Number of core(s) to be used for calculation:",getDoParWorkers(),"\n")
#cl_1core = makeCluster(1)
#registerDoParallel(cl_1core)

#We take spot rate data as of 30-May-2014
valuation_date = as.Date("2014-05-30","%Y-%m-%d")
input_data = HistoricalForwardCurve[HistoricalForwardCurveDate == valuation_date,]/100
#input_data = read.csv("P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/data/spot_curve.csv", header = TRUE)$rate
#dX_data = read.csv("P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/data/DX.csv", header = TRUE)
#dX_data = read.delim("clipboard")

timestamp = Sys.time()

#NumberOfMaturity = nrow(input_data)
MaxMaturity = 1
maturityBucket = 0.5
NumberOfYear = 2 #projection of the forward rates evolation over 10 years
timestep = 0.01 #size of timestep for projection
NumberOfTimesteps = NumberOfYear/timestep
NumberSimulation = 10000

#pre-calculation (performance)
MaturityList = seq(0,MaxMaturity,by=maturityBucket) #1M rate is taken as proxy for Maturity=0
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
  
  mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
  #initialize first row (equivalent to t=0) with input data (latest spot curve)
  #mat[1,] = input_data$rate
  mat[1,] = input_data[1:length(MaturityList)]

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
        #if (i>=2 && i<5) {
        #  cat(mat[i-1,j],drift[j]*timestep,sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3]),sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep),((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep,"\n")
        #}
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
  
  #calculate value
  #we sum the f(t) for maturity 0.08 (1M) which we can as approximation for r(t)
  #1:100 because 1Y bond
  #Result[k] = exp(-1*sum(mat[1:100,1]*timestep))
  res = exp(-1*sum((mat[1:100,1])*timestep))

  #if (k %% 100 == 0 || k == 10) {
  #  ConvergenceDiagram[l,"nbsimul"] = k
  #  ConvergenceDiagram[l,"value"] = mean(Result[1:k])
  #  l = l+1
  #}

}

stopCluster(cl)
#stopImplicitCluster()



BondPrice= mean(Result)
cat("1Y Bond Price:",BondPrice,"\n")
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







