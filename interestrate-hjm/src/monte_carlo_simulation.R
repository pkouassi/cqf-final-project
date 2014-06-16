
#Trapezium rule
integral_trapezium_rule = function(f,x0,xn,N) {
  dx = (xn-x0)/N
  
  #0.5 * dx * ((y0 + yn) + 2 (y1 + y2 + ... + yn-1))
  result = 0
  
  #yo / yn
  #cat("y0:",f(x0),"\n")
  #cat("yn:",f(xn),"\n")
  result = result + 0.5 * f(x0) + 0.5 * f(xn)
  
  #y1 ... yn-1
  for (i in seq(1,N-1)) {
    #cat("y(",x0 + i*dx,"):",f(x0 + i*dx),"\n")
    result = result + f(x0 + i*dx)  
  }
  
  #multiply by dx
  result = result * dx
  
  return(result)
}

f = function(x) sqrt((2*x+1))

integral_trapezium_rule(f,0,1,4)

vol_1 = function(Tau) {
  return(0.0064306548)
}

vol_2 = function(Tau) {
  return(-0.0035565431 + Tau*(-0.0005683999) + Tau^2 * 0.0001181915 + Tau^3 * (-0.0000035939))
}

vol_3 = function(Tau) {
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
    M1 = integral_trapezium_rule(vol_1,0,Tau,NumberOfSlice)
    M2 = integral_trapezium_rule(vol_2,0,Tau,NumberOfSlice)
    M3 = integral_trapezium_rule(vol_3,0,Tau,NumberOfSlice)
    
    return(vol_1(Tau)*M1+vol_2(Tau)*M2+vol_3(Tau)*M3)
  }
}

input_data = read.csv("P:\\CQF\\FinalProject\\git-root\\finalproject\\interestrate-hjm\\data\\spot_curve.csv", header = TRUE)
#dX_data = read.csv("P:\\CQF\\Module4\\Lecture 5\\DX.csv", header = TRUE)

NumberOfMaturity = nrow(input_data)
maturityBucket = 0.5
NumberOfYear = 10 #projection of the forward rates evolation over 10 years
timestep = 0.01 #size of timestep for projection
NumberOfTimesteps = NumberOfYear/timestep

mat = matrix(NA, 
      nrow=(NumberOfTimesteps+1),
      ncol=NumberOfMaturity,
      byrow = TRUE);

#initialize first row (equivalent to t=0) with input data (latest spot curve)
mat[1,] = input_data$rate

#set.seed(165468833)
dX_1 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
dX_2 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
dX_3 = rnorm(NumberOfTimesteps,mean = 0,sd =1)
#dX_1 = dX_data$dX_1
#dX_2 = dX_data$dX_2
#dX_3 = dX_data$dX_3

rbind(drift,volatility_1,volatility_2,volatility_3)

#i: timestep for projection
#j: maturity of the curve

#pre-calculation (performance)
Maturity = seq(0,25,by=maturityBucket)
drift = rep(NA,ncol(mat))
volatility_1 = rep(NA,ncol(mat))
volatility_2 = rep(NA,ncol(mat))
volatility_3 = rep(NA,ncol(mat))

for (j in seq(1,ncol(mat))) {
  drift[j] = M(Maturity[j])
  volatility_1[j] = vol_1(Maturity[j])
  volatility_2[j] = vol_2(Maturity[j])
  volatility_3[j] = vol_3(Maturity[j])
}


for (i in seq(2,nrow(mat))) {
  for (j in seq(1,ncol(mat))) {
    #Equation: F + drift*dt+SUM(vol*dX_i)*SQRT(dt)+dF/dtau*dt
    if (j<ncol(mat)) {
      mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
        sum(volatility_1[j]*dX_1[i],volatility_2[j]*dX_2[i],volatility_3[j]*dX_3[i])*sqrt(timestep) +
        ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
      
    }
    else if (j == ncol(mat)) {
      #use backward difference for dF/dTau on last column
      mat[i,j] = mat[i-1,j] + drift[j]*timestep + 
        sum(volatility_1[j]*dX_1[i],volatility_2[j]*dX_2[i],volatility_3[j]*dX_3[i])*sqrt(timestep) +
        ((mat[i-1,j]-mat[i-1,j-1])/(maturityBucket))*timestep 
    }
    
  }
}

plot_x_lim = c(0,25);
plot_y_lim = c(0,0.08);
plot(Maturity,mat[1,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",
     main="Forward Rates Term Structure (Zero curve)",xlab="Maturity, year",ylab="")
par(new=TRUE);
plot(Maturity,mat[100,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="pink",
     main="",xlab="",ylab="")
par(new=TRUE);
plot(Maturity,mat[500,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="yellow",
     main="",xlab="",ylab="")
par(new=TRUE);
plot(Maturity,mat[1000,],type="l",lwd=2,xlim=plot_x_lim, ylim=plot_y_lim,col ="cyan",
     main="",xlab="",ylab="")





