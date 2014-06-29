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
NumberSimulation = 10000

#pre-calculation (performance)
MaturityList = seq(1/12,MaxMaturity,by=maturityBucket) #1M rate is taken as proxy for Maturity=0
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

populate_row = function(i,mat,dX) {
  result = rep(NA,ncol(mat))
  #cat("row above:",mat[i-1,],"\n")
  #cat("j:",seq(1,ncol(mat)-1),"\n")
  for (j in seq(1,ncol(mat)-1)) 
  {
    #Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
    result[j] = mat[i-1,j] + drift[j]*timestep + 
        sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
        ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
  }
  
  #Last row 
  #use backward difference for dF/dTau on last column
  result[ncol(mat)] = mat[i-1,j] + drift[j]*timestep + 
      sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
      ((mat[i-1,j]-mat[i-1,j-1])/(
        maturityBucket))*timestep 
      
  return(result)
}  
populate_row.compiled = cmpfun(populate_row)


dX_Sobol = quasirandom.nag(NumberSimulation,3*NumberOfTimesteps,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
#dX_Sobol = rnorm.sobol(n = NumberSimulation, dimension = 3*NumberOfTimesteps , scrambling = 3)
Result = foreach(k=1:NumberSimulation, .combine=rbind) %dopar% {
#for (k in seq(1,NumberSimulation)) {
  #cat(k,"...\n")
  if (k%%(NumberSimulation/20) == 0) cat((k/NumberSimulation)*100,"% ...\n")
  
  #mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
  mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
  #initialize first row (equivalent to t=0) with input data (latest spot curve)
  #mat[1,] = input_data$rate
  
  mat[1,] = input_data[1:length(MaturityList)]
  #mat[1,] = input_data[1:length(MaturityList)]

  dX = matrix(dX_Sobol[k,],ncol=3,nrow=NumberOfTimesteps,byrow = TRUE)
  #dX = dX_data
  #dX = matrix(rnorm(NumberOfTimesteps*3,mean = 0,sd =1),ncol=3,nrow=NumberOfTimesteps,byrow = FALSE)
  
  #i: timestep for projection
  #j: maturity of the curve
  
  #populate matrix
#   for (i in seq(2,nrow(mat))) {
#     
#     for (j in seq(1,ncol(mat))) {
#       #Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
#       if (j<ncol(mat)) {
#         mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
#           sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
#           ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
#         
#       }
#       else if (j == ncol(mat)) {
#         #use backward difference for dF/dTau on last column
#         mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
#           sum(volatility_1[j]*dX[i-1,1],volatility_2[j]*dX[i-1,2],volatility_3[j]*dX[i-1,3])*sqrt(timestep) +
#           ((mat[i-1,j]-mat[i-1,j-1])/(
#             maturityBucket))*timestep 
#       }
#       
#     }
#   }
  
  #populate matrix
  #cat("nrow/ncol (before):",nrow(mat),ncol(mat),"\n")
  #print(populate_row.compiled(2,mat))
  #mat_populated = t(sapply(seq(2,nrow(mat)),populate_row.compiled,mat=mat))
  #print(mat_populated)
  for (i in seq(2,nrow(mat))) {
    mat[i,] =  populate_row.compiled(i,mat,dX)
  }
  
  #cat("nrow/ncol (after):",nrow(mat),ncol(mat),"\n")
  #print(mat[1:10,])
  #print(mat[101,1:12])
  
  #Calculate value of a 1Y and 2Y bond
  Bond1Y = ComputeBondPrice(mat,timestep,0,1)
  Bond2Y = ComputeBondPrice(mat,timestep,0,2)
  
  #Calculate value of LIBOR (continuous compounding and 3M compounding)
  LiborContinuouslyCompounded = ComputeLIBORRates(mat,timestep,1,seq(1.25,5.00,by=0.25))
  Libor3MCompounded = 4*(exp(LiborContinuouslyCompounded/4)-1)
  
  #Calculate value of Caplet with strike k(1year forward starting 3M duration)
  Cap1by2_0.10 = ComputeCapPrice(mat,timestep,1,2,0.0010)
  Cap1by2_0.25 = ComputeCapPrice(mat,timestep,1,2,0.0025)
  Cap1by2_0.50 = ComputeCapPrice(mat,timestep,1,2,0.0050)
  Cap1by2_0.75 = ComputeCapPrice(mat,timestep,1,2,0.0075)
  Cap1by2_1.00 = ComputeCapPrice(mat,timestep,1,2,0.0100)
  Cap1by2_1.50 = ComputeCapPrice(mat,timestep,1,2,0.0150)
  Cap1by2_2.00 = ComputeCapPrice(mat,timestep,1,2,0.0200)
  Cap1by2_2.50 = ComputeCapPrice(mat,timestep,1,2,0.0250)
  Cap1by2_3.00 = ComputeCapPrice(mat,timestep,1,2,0.0300)
  Cap1by2_4.00 = ComputeCapPrice(mat,timestep,1,2,0.0400)
  Cap1by2_5.00 = ComputeCapPrice(mat,timestep,1,2,0.0500)
  
  #cat("**************************************************\n")
  #LiborContinuouslyCompounded_t0 = ComputeLIBORRates(mat,timestep,0,c(0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75))
  #Libor3MCompounded_t0 = 4*(exp(LiborContinuouslyCompounded_t0/4)-1)
  #print(rbind(c(0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75),LiborContinuouslyCompounded_t0))

  #Cap1by2_0.50 = ComputeCapPriceDebug(mat,timestep,0,1,0.0050)
  Cap1by3_2.00 = ComputeCapPrice(mat,timestep,1,3,0.0200)
  Cap1by3.5_2.00 = ComputeCapPrice(mat,timestep,1,3.5,0.0200)
  Cap1by4_2.00 = ComputeCapPrice(mat,timestep,1,4,0.0200)
  Cap1by4.5_2.00 = ComputeCapPrice(mat,timestep,1,4.5,0.0200)
  Cap1by5_2.00 = ComputeCapPrice(mat,timestep,1,5,0.0200)  
  
  #surface
  strike_array = c(0.0050,0.0100,0.0150,0.0200,0.0250,0.0300,0.0350,0.0400)
  maturity_array = c(2,3,4,5)
  p=1
  CapVolSurface = rep(NA,length(strike_array)*length(maturity_array))
  for (strike in strike_array) {
    for (maturity in maturity_array) {
      CapVolSurface[p] = ComputeCapPrice(mat,timestep,1,maturity,strike)
      p = p+1
    }
  }  
  
  #define the vector of values which will be kept for all simulations  
  res = c(bond1=Bond1Y,bond2=Bond2Y,libor=Libor3MCompounded,cap1=Cap1by2_0.10,cap2=Cap1by2_0.25,cap3=Cap1by2_0.50,cap4=Cap1by2_0.75,cap5=Cap1by2_1.00,cap6=Cap1by2_1.50,cap7=Cap1by2_2.00,cap8=Cap1by2_2.50,cap9=Cap1by2_3.00,cap10=Cap1by2_4.00,cap11=Cap1by2_5.00,cap12=Cap1by3_2.00,cap13=Cap1by3.5_2.00,cap14=Cap1by4_2.00,cap15=Cap1by4.5_2.00,cap16=Cap1by5_2.00,capvolsurf=CapVolSurface)
  
  #if (k %% 100 == 0 || k == 10) {
  #  ConvergenceDiagram[l,"nbsimul"] = k
  #  ConvergenceDiagram[l,"value"] = mean(Result[1:k])
  #  l = l+1
  #}

}
cat("Time to compute:", Sys.time()-timestamp,"\n")
stopCluster(cl)


Bond1Y = mean(Result[,"bond1"])
Bond2Y = mean(Result[,"bond2"])
cat("Bond 1Y:",Bond1Y,"\n")
cat("Bond 2Y:",Bond2Y,"\n")

Libor_1.25 = mean(Result[,"libor1"])
Libor_1.50 = mean(Result[,"libor2"])
Libor_1.75 = mean(Result[,"libor3"])
Libor_2.00 = mean(Result[,"libor4"])
Libor_2.25 = mean(Result[,"libor5"])
Libor_2.50 = mean(Result[,"libor6"])
Libor_2.75 = mean(Result[,"libor7"])
Libor_3.00 = mean(Result[,"libor8"])
Libor_3.25 = mean(Result[,"libor9"])
Libor_3.50 = mean(Result[,"libor10"])
Libor_3.75 = mean(Result[,"libor11"])
Libor_4.00 = mean(Result[,"libor12"])
Libor_4.25 = mean(Result[,"libor13"])
Libor_4.50 = mean(Result[,"libor14"])
Libor_4.75 = mean(Result[,"libor15"])
Libor_5.00 = mean(Result[,"libor16"])

cat("LIBOR at t=1.25:",Libor_1.25,"\n")
cat("LIBOR at t=1.50:",Libor_1.50,"\n")
cat("LIBOR at t=1.75:",Libor_1.75,"\n")
cat("LIBOR at t=2:",Libor_2.00,"\n")

#Caps price to observe volatility smile
Cap1by2_0.10 = mean(Result[,"cap1"])
Cap1by2_0.25 = mean(Result[,"cap2"]) #1Y by 2Y is a cap starting in one year and matures in 2 years
Cap1by2_0.50 = mean(Result[,"cap3"])
Cap1by2_0.75 = mean(Result[,"cap4"])
Cap1by2_1.00 = mean(Result[,"cap5"])
Cap1by2_1.50 = mean(Result[,"cap6"])
Cap1by2_2.00 = mean(Result[,"cap7"])
Cap1by2_2.50 = mean(Result[,"cap8"])
Cap1by2_3.00 = mean(Result[,"cap9"])
Cap1by2_4.00 = mean(Result[,"cap10"])
Cap1by2_5.00 = mean(Result[,"cap11"])

#Caps price to observe term structure of volatility
Cap1by3_2.00 = mean(Result[,"cap12"])
Cap1by3.5_2.00 = mean(Result[,"cap13"])
Cap1by4_2.00 = mean(Result[,"cap14"])
Cap1by4.5_2.00 = mean(Result[,"cap15"])
Cap1by5_2.00 = mean(Result[,"cap16"])

#-------------------------------------------------
Cap1by1Premiums = c(Cap1by2_0.10,Cap1by2_0.25,Cap1by2_0.50,Cap1by2_0.75,Cap1by2_1.00,Cap1by2_1.50,Cap1by2_2.00,Cap1by2_2.50,Cap1by2_3.00,Cap1by2_4.00,Cap1by2_5.00)
Cap1by1Strikes = c(0.0010,0.0025,0.0050,0.0075,0.0100,0.0150,0.0200,0.0250,0.0300,0.0400,0.0500)

plot(Cap1by1Strikes,Cap1by1Premiums,type="l",xlab="Strike",ylab="Premium",main="Premium as a function of Strike")

Cap1by1IVs = rep(NA,length(Cap1by1Strikes))
for (i in seq(1,length(Cap1by1Strikes))) {
  Cap1by1IVs[i] = Black76CapImpliedVolatility(1,2,Cap1by1Strikes[i],c(Libor_1.25,Libor_1.50,Libor_1.75,Libor_2.00),Cap1by1Premiums[i])
}

cat("Cap Premium and IVs:\n")
rbind(Cap1by1Strikes,Cap1by1Premiums,Cap1by1IVs)

plot(Cap1by1Strikes,Cap1by1IVs,type="l",xlab="Strike",ylab="Volatility",main="Implied Volatility as a function of Strike")

#-------------------------------------------------
CapTermStructureTenors = c(2,3,3.5,4,4.5,5)
CapTermStructurePremiums = c(Cap1by2_2.00,Cap1by3_2.00,Cap1by3.5_2.00,Cap1by4_2.00,Cap1by4.5_2.00,Cap1by5_2.00)
CapTermStructureIVs = rep(NA,length(CapTermStructureTenors))
Libor = c(Libor_1.25,Libor_1.50,Libor_1.75,Libor_2.00,Libor_2.25,Libor_2.50,Libor_2.75,Libor_3.00,Libor_3.25,Libor_3.50,Libor_3.75,Libor_4.00,Libor_4.25,Libor_4.50,Libor_4.75,Libor_5.00)

for (i in seq(1,length(CapTermStructureTenors))) {
    cat("i=",i,"\n")
    Libor_list = Libor[1:((CapTermStructureTenors[i]-1)/0.25)]
    print(Libor_list)
    CapTermStructureIVs[i] = Black76CapImpliedVolatility(1,CapTermStructureTenors[i],0.02,Libor_list,CapTermStructurePremiums[i])
}

plot(CapTermStructureTenors,CapTermStructureIVs,type="l",xlab="Tenors (Expiry)",ylab="Volatility",main="Implied Volatility as a function of Expiry")

#-------------------------------------------------
#Caps price to observe full volatility surface (strike/maturity)

CapVolSurfaceStrikes = c(0.0050,0.0100,0.0150,0.0200,0.0250,0.0300,0.0350,0.0400)
CapVolSurfaceMaturities = c(2,3,4,5)
CapVolSurfacePremiums = rep(NA,length(CapVolSurfaceStrikes)*length(CapVolSurfaceMaturities))
for (i in seq(1,length(CapVolSurfaceStrikes)*length(CapVolSurfaceMaturities))) {
  CapVolSurfacePremiums[i] = mean(Result[,paste("capvolsurf",i,sep="")])
}
CapVolSurfacePremiumsMatrix = matrix(CapVolSurfacePremiums, nrow=length(CapVolSurfaceStrikes), ncol=length(CapVolSurfaceMaturities),byrow=TRUE)

CapVolSurfaceIVsMatrix = matrix(NA, nrow=length(CapVolSurfaceStrikes), ncol=length(CapVolSurfaceMaturities),byrow=TRUE)

Libor = c(Libor_1.25,Libor_1.50,Libor_1.75,Libor_2.00,Libor_2.25,Libor_2.50,Libor_2.75,Libor_3.00,Libor_3.25,Libor_3.50,Libor_3.75,Libor_4.00,Libor_4.25,Libor_4.50,Libor_4.75,Libor_5.00)

for (j in seq(1,length(CapVolSurfaceMaturities))) {
  cat("j=",j,"\n")
  Libor_list = Libor[1:((CapVolSurfaceMaturities[j]-1)/0.25)]
  print(Libor_list)
  
  for (i in seq(1,length(CapVolSurfaceStrikes))) {
    cat("Maturity/Strike:",CapVolSurfaceMaturities[j],"/",CapVolSurfaceStrikes[i],"\n")
    CapVolSurfaceIVsMatrix[i,j] = Black76CapImpliedVolatility(1,CapVolSurfaceMaturities[j],CapVolSurfaceStrikes[i],Libor_list,CapVolSurfacePremiumsMatrix[i,j])  
  }
}

persp(CapVolSurfaceStrikes*100, CapVolSurfaceMaturities, CapVolSurfaceIVsMatrix*100 ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Volatility", xlim=c(0,4),ylim=c(2,5),zlim=c(0,100))

legend(70, 1.2, c("Binary Call", "Bull Call Spread"), col = c("blue","red"),
       text.col = "black", lty = c(1, 2), pch = c(NA, NA),
       merge = TRUE, bg = "gray90")

#Convergence Diagrams

#------------------------
# Bond 1Y
#------------------------
l=1
nbobservation = 100
ConvergenceDiagramBond1Y = matrix(NA, ncol=2, nrow=(NumberSimulation/nbobservation)+1)
colnames(ConvergenceDiagramBond1Y) = c("nbsimul", "value")
for (i in seq(1,NumberSimulation)) {
  if (i %% nbobservation == 0 || i == 10) {
    ConvergenceDiagramBond1Y[l,"nbsimul"] = i
    ConvergenceDiagramBond1Y[l,"value"] = mean(Result[1:i,"bond1"])
    l = l+1
  }
}
plot(ConvergenceDiagramBond1Y[,"nbsimul"],ConvergenceDiagramBond1Y[,"value"],type="l",xlab="Number of Simulations",ylab="Bond Price",main="1Y Bond - Convergence Diagram")

#ConvergenceDiagramBond1Y.rnorm = ConvergenceDiagramBond1Y
#ConvergenceDiagramBond1Y.sobol = ConvergenceDiagramBond1Y

matplot(ConvergenceDiagramBond1Y.rnorm[,"nbsimul"],cbind(ConvergenceDiagramBond1Y.rnorm[,"value"],ConvergenceDiagramBond1Y.sobol[,"value"]),type="l",xlab="Number of Simulations",ylab="Bond Price",main="1Y Bond - Convergence Diagram", col=c("blue","red"),lwd=1,lty=1)

#------------------------
# Libor at t=1Y
#------------------------

l=1
nbobservation = 100
ConvergenceDiagramLibor1Y = matrix(NA, ncol=2, nrow=(NumberSimulation/nbobservation)+1)
colnames(ConvergenceDiagramLibor1Y) = c("nbsimul", "value")
for (i in seq(1,NumberSimulation)) {
  if (i %% nbobservation == 0 || i == 10) {
    ConvergenceDiagramLibor1Y[l,"nbsimul"] = i
    ConvergenceDiagramLibor1Y[l,"value"] = mean(Result[1:i,"libor1"])
    l = l+1
  }
}
plot(ConvergenceDiagramLibor1Y[,"nbsimul"],ConvergenceDiagramLibor1Y[,"value"],type="l",xlab="Number of Simulations",ylab="LIBOR Rate",main="LIBOR at t=1Y - Convergence Diagram")

#ConvergenceDiagramLibor1Y.sobol = ConvergenceDiagramLibor1Y
#ConvergenceDiagramLibor1Y.rnorm = ConvergenceDiagramLibor1Y

#matplot(ConvergenceDiagramLibor1Y.rnorm[,"nbsimul"],cbind(ConvergenceDiagramLibor1Y.rnorm[,"value"],ConvergenceDiagramLibor1Y.sobol[,"value"]),type="l",xlab="Number of Simulations",ylab="LIBOR Rate",main="LIBOR at t=1Y - Convergence Diagram", col=c("blue","red"),lwd=1,lty=1)


