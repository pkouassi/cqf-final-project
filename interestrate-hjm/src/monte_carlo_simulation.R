#ans_hjm1 = HeathJarrowMortonPricing("bond",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,10,"nag-sobol")

#ans_hjm1.rnorm = ans_hjm1
#ans_hjm1.sobol = ans_hjm1

#convergence diagram
# step=10
# nbobservations = round(10000/step)
# bond_expectation = rep(NA,nbobservations)
# for (i in seq(1,nbobservations)) bond_expectation[i] = mean(ans_hjm1$simulation[1:i*step,1])
# plot(seq(5,nbobservations)*step,bond_expectation[5:nbobservations],type="l")
# 
# 

nbsim_50000 = 10
nbsim_10000 = 10

maturity_list = c(2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5)
strike_list = c(0.001,0.002,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04)
timestamp = Sys.time()
ans_hjm2 = HeathJarrowMortonPricing("cap",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-sobol")
cat("time to complete",Sys.time()-timestamp,"\n")
#persp(strike_list*100, maturity_list, ans_hjm2$iv*100 ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Volatility")

#ans_hjm3 = HeathJarrowMortonPricing("cap",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-niederreiter")

ans_hjm4 = HeathJarrowMortonPricing("cap",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"rnorm")

ans_hjm5 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-sobol")
ans_hjm6 = HeathJarrowMortonPricing("swap",2,c(3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_10000,"nag-sobol")
ans_hjm7 = HeathJarrowMortonPricing("swap",3,c(4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_10000,"nag-sobol")

ans_hjm8 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"rnorm")

ans_hjm9 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-sobol")

ans_hjm10 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-niederreiter")

ans_hjm11 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"rnorm")


# 
# ans_hjm3 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,100,"nag-sobol")
# 
# 
# ans_hjm4 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,100,"nag-sobol")
# 
# persp(strike_list*100, maturity_list, ans_hjm4$price ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Price")



