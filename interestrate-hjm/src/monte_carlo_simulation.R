#parallel computing set-up
cl = makeCluster(detectCores())
registerDoParallel(cl)
cat("Number of core(s) to be used for calculation:",getDoParWorkers(),"\n")
#cl_1core = makeCluster(1)
#registerDoParallel(cl_1core)

input_data = ValuationDateForwardCurve$rate/100
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
NumberSimulation = 100

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

#Cap strike
K1 = 0.03

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
          ((mat[i-1,j]-mat[i-1,j-1])/(
            maturityBucket))*timestep 
      }
      
    }
  }
  
  #calculate value of a 1Y bond
  #we sum the f(t) for maturity 0.08 (1M) which we can as approximation for r(t)
  #1:100 because 1Y bond
  #Result[k] = exp(-1*sum(mat[1:100,1]*timestep))
  #bond1Y = exp(-1*sum((mat[1:101,1])*timestep))
  bond1Y = ComputeBondPrice(mat,timestep,0,1)
  bond2Y = exp(-1*sum((mat[1:201,1])*timestep))
  
  #calculate value of Caplet with strike k(1year forward starting 3M duration)
  
  #Project Workshop suggests simple averaging but it will be an advantage if you can choose
  #and apply one of the simple methods of forward curve interpolation from Hagan & West,
  #given that BOE has already applied its own VRP curve-ftting methodology.
  #one year caplet
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
  libor_test_1y_cont_compounded = integrate(tmpfunc,0,1)$value
  libor_test_1y_simply_compounded = 1*(exp(libor_test_1y_cont_compounded)-1)
  
  #libor at time t=1Y
  libor_1y_cont_compounded = integrate(tmpfunc,0,1)$value
  libor_1y_simply_compounded = 4*(exp(libor_1y_cont_compounded/4)-1)

  #libor at time t=1.25Y
  libor_1.25y_cont_compounded = integrate(tmpfunc,0,1.25)$value
  libor_1.25y_simply_compounded = 4*(exp(libor_1.25y_cont_compounded/4)-1)
  
  #libor at time t=1.5Y
  libor_1.5y_cont_compounded = integrate(tmpfunc,0,1.5)$value
  libor_1.5y_simply_compounded = 4*(exp(libor_1.5y_cont_compounded/4)-1)
  
  #libor at time t=1.75Y
  libor_1.75y_cont_compounded = integrate(tmpfunc,0,1.75)$value
  libor_1.75y_simply_compounded = 4*(exp(libor_1.75y_cont_compounded/4)-1)
  
  libor_1.75y_cont_compounded_verif = sum(mat[101,1:21]*(1/12))
  
  caplet_1_1.25 = max(libor_1y_simply_compounded-K1,0)*GetDiscountFactor(ValuationDateOISYieldCurve,1.25)*0.25
  caplet_1.25_1.5 = max(libor_1.25y_simply_compounded-K1,0)*GetDiscountFactor(ValuationDateOISYieldCurve,1.5)*0.25
  caplet_1.5_1.75 = max(libor_1.5y_simply_compounded-K1,0)*GetDiscountFactor(ValuationDateOISYieldCurve,1.75)*0.25
  caplet_1.75_2 = max(libor_1.75y_simply_compounded-K1,0)*GetDiscountFactor(ValuationDateOISYieldCurve,2)*0.25
  
  cap_1yfwd_1y = caplet_1_1.25 + caplet_1.25_1.5 + caplet_1.5_1.75 + caplet_1.75_2
  cap_test_1yfwd_1y = max(libor_test_1y_simply_compounded-K1,0)*GetDiscountFactor(ValuationDateOISYieldCurve,2)*1
  
  #caplet1x1_005 = max(libor_simply_coupounded-0.005,0)*bond1Y*1
  #caplet1x1_002 = max(libor_simply_coupounded-0.002,0)*bond1Y*1
  
  res = list(bond1Y=bond1Y,captlet=caplet_1_1.25)
  
  #res = c(bond1Y,caplet_1_1.25,caplet_1.25_1.5,caplet_1.5_1.75,caplet_1.75_2,cap_1yfwd_1y,libor_1y_simply_compounded,libor_1.25y_cont_compounded,libor_1.5y_cont_compounded,libor_1.75y_cont_compounded,mat[101,12],mat[101,24],libor_1.75y_cont_compounded_verif,cap_test_1yfwd_1y)
  
  
  #if (k %% 100 == 0 || k == 10) {
  #  ConvergenceDiagram[l,"nbsimul"] = k
  #  ConvergenceDiagram[l,"value"] = mean(Result[1:k])
  #  l = l+1
  #}

}

stopCluster(cl)
#stopImplicitCluster()

ComputeBondPrice = function(matrix,timestep,t, T) {
  #row 1 of the matrix is the valuation forward curve
  #row 2 is forward curve for valuation date + 1*timestep
  #row 3 is forward curve for valuation date + 2*timestep
  #first column of the matrix is the 1M forward which we use as proxy for r(t) (spot rate)
  #Compute the bond price which start at time t and matures at time T 
  #by integrating over the first column 
  start_index = t/timestep+1
  end_index = T/timestep+1  
  cat("start_index:",start_index,"\n")
  cat("end_index:",end_index,"\n")  
  return(exp(-1*sum((matrix[start_index:end_index,1])*timestep)))  
}




BondPrice= mean(unlist(Result[,"bond1Y"]))
cat("1Y Bond Price:",BondPrice,"\n")

cat("Caplet 3M starts at t=1 with K = ",K1," Price:",mean(Result[,2]),"\n")
cat("Caplet 3M starts at t=1.25 with K = ",K1," Price:",mean(Result[,3]),"\n")
cat("Caplet 3M starts at t=1.5 with K = ",K1," Price:",mean(Result[,4]),"\n")
cat("Caplet 3M starts at t=1.75 with K = ",K1," Price:",mean(Result[,5]),"\n")
cat("Cap 1Y starts at t=1 with K = ",K1," Price:",mean(Result[,6]),"\n")
cat("LIBOR at t=1:",mean(Result[,7]),"\n")
cat("LIBOR at t=1.25:",mean(Result[,8]),"\n")
cat("LIBOR at t=1.5:",mean(Result[,9]),"\n")
cat("LIBOR at t=1.75:",mean(Result[,10]),"\n")
cat("fwd expectation at t=1:",mean(Result[,11]),"\n")
cat("fwd expectation at t=2:",mean(Result[,12]),"\n")
cat("LIBOR at t=1.75 (verif simple integration):",mean(Result[,13]),"\n")
cat("Caplet 1Y start at t=1 (simple):",mean(Result[,14]),"\n")

#comparer avec formule de la spreadsheet
#Cap_1Y = Black76ImpliedVolatilityBisection("call",mean(Result[,6]),0.03,1,r,Premium, SigmaMin, SigmaMax, Iteration, MaxIteration, MaxError)

#cat("1x1 Caplet with K = 0.008 Price:",mean(Result[,2]),"\n")
#cat("1x1 Caplet with K = 0.005 Price:",mean(Result[,3]),"\n")
#cat("1x1 Caplet with K = 0.002 Price:",mean(Result[,4]),"\n")
cat("Time to compute:", Sys.time()-timestamp,"\n")

CplRate = 0.057319543; CplStrike = 0.04; CplTau = 1; DF = 0.906137674
Black = Black76OptionPricing("call",CplRate,CplStrike,1,0.32000659247506)
CapletBlack = Black*DF*CplTau/(1+CplRate*CplTau)



myfunc = function(x) {
  CplRate = 0.057319543
  CplStrike = 0.04
  CplTau = 1
  DF = 0.906137674
  CplPremium = 0.01569389
  value = Black76OptionPricing("call",CplRate,CplStrike,1,x)*DF*CplTau/(1+CplRate*CplTau)-CplPremium
  return(value)
}

myfunc2 = function(x) {
  CplRate = 0.02537436
  CplStrike = 0.03
  CplTau = 0.25
  DF = GetDiscountFactor(ValuationDateOISYieldCurve,1.75)
  CplPremium = 0.002709545
  value = Black76OptionPricing("call",CplRate,CplStrike,1,x)*DF*CplTau/(1+CplRate*CplTau)-CplPremium
  return(value)
}

#plot strikes vs. cap price
plot(c(0.03,0.025,0.02,0.015,0.01,0.005,0.004,0.003),c(0.002371903,0.003965611,0.006264865,0.009419878,0.01337705,0.01793497,0.01887717,0.02078812),type="l")

cap1Yx1YPrice = function(sigma) {
  CplStrike = 0.005
  CplTau = 0.25
  CplRate = c(0.01495364,0.0198774,0.02526753,0.03105881)
  #CapPremium = 0.006264865 //0.03
  CapPremium = 0.01793497
  
  value = 0
  for (i in seq(1,length(CplRate))) {
    value = value + Black76OptionPricing("call",CplRate[i],CplStrike,1+(i-1)*0.25,sigma)*GetDiscountFactor(ValuationDateOISYieldCurve,1+i*0.25)*CplTau/(1+CplRate[i]*CplTau)
  }
  
  value = value - CapPremium
  
  return(value)
}
uniroot(cap1Yx1YPrice,lower=0,upper=1.5)

#uniroot(myfunc,lower=0,upper=1)

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







