#==============================================================================
# title           :monte_carlo_simulation.R
# description     :Call Core function that price bonds, caps, swaptions
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

#==============================================================================
# Calculation of forward starting bond price- 1000 simulations
#==============================================================================

# Requires NAG library
#ans_hjm1 = HeathJarrowMortonPricing("bond",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,1000,"nag-sobol")

ans_hjm1 = HeathJarrowMortonPricing("bond",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,1000,"rnorm")

# convergence diagram for Bond price
step=1
nbobservations = round(1000/step)
bond_expectation = rep(NA,nbobservations)
for (i in seq(1,nbobservations)) bond_expectation[i] = mean(ans_hjm1$simulation[1:i*step,1])
plot(seq(5,nbobservations)*step,bond_expectation[5:nbobservations],type="l",ylab="Bond price",xlab="Number of simulations")

# Requires NAG library
#ans_hjm2 = HeathJarrowMortonPricing("bond",2,c(3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,1000,"nag-sobol")
ans_hjm2 = HeathJarrowMortonPricing("bond",2,c(3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,1000,"rnorm")

#==============================================================================
# Cap pricing - 100 simulations (to keep it fast for the demo)
#==============================================================================
maturity_list = c(2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5)
strike_list = c(0.001,0.002,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04)
nbsim = 1000

# Cap pricing using Sobol
ans_hjm3 = HeathJarrowMortonPricing("cap",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim,"sobol")
nbsim = 100

# Convergence diagram
step=1
nbobservations = round(1000/step)
cap_expectation = rep(NA,nbobservations)
for (i in seq(1,nbobservations)) cap_expectation[i] = mean(ans_hjm3$simulation[1:i*step,54])
plot(seq(5,nbobservations)*step,cap_expectation[5:nbobservations],type="l",ylab="Cap price",xlab="Number of simulations")

# Plot cap volatility skew (price in basis point)
matplot(strike_list*100,ans_hjm3$price[,c(1,5,9,13)]*10000,type="l",xlab="Strike",ylab="Price (bp)",col=c("black","blue","red","chartreuse3"),lty=1)
legend(3.5, 650, c("2Y","3Y","4Y","5Y"), col = c("black","blue","red","chartreuse3"), text.col = "black", lty = 1,merge = TRUE, bg = "gray90")

# Plot volatility surface
persp(strike_list*100, maturity_list, ans_hjm3$iv*100 ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Volatility")

# Cap pricing using rnorm
ans_hjm4 = HeathJarrowMortonPricing("cap",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim,"rnorm")

# Cap pricing using nag-sobol
# Requires NAG
#ans_hjm5 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim,"nag-sobol")

#==============================================================================
# Swap pricing - 100 simulations (to keep it fast for the demo)
#==============================================================================
ans_hjm7 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim,"sobol")

#==============================================================================
# Swaption pricing - 100 simulations (to keep it fast for the demo)
#==============================================================================
maturity_list = c(2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5)
strike_list = c(0.001,0.002,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04)
nbsim = 1000

# Swaption pricing using sobol
ans_hjm8 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim,"sobol")
nbsim = 100

# Convergence diagram
step=1
nbobservations = round(1000/step)
swaption_expectation = rep(NA,nbobservations)
for (i in seq(1,nbobservations)) swaption_expectation[i] = mean(ans_hjm8$simulation[1:i*step,31])
plot(seq(5,nbobservations)*step,swaption_expectation[5:nbobservations],type="l",ylab="Swaption price",xlab="Number of simulations")

# Plot swaption "volatility" surface (in price)
persp(strike_list*100, maturity_list, ans_hjm8$price*10000 ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Price")

# Swaption pricing using rnorm
ans_hjm9 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim,"rnorm")




