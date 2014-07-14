
timestamp = Sys.time()

cat("time to complete",Sys.time()-timestamp,"\n")




timestamp = Sys.time()
ans_hjm5 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-sobol")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
ans_hjm6 = HeathJarrowMortonPricing("swap",2,c(3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_10000,"nag-sobol")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
ans_hjm7 = HeathJarrowMortonPricing("swap",3,c(4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_10000,"nag-sobol")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
ans_hjm8 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"rnorm")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
ans_hjm9 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"sobol")
cat("time to complete",Sys.time()-timestamp,"\n")

matplot(strike_list*100,ans_hjm9$price[,c(1,5,9,13)]*10000,type="l",xlab="Strike",ylab="Price (bp)",col=c("black","blue","red","chartreuse3"),lty=1)
legend(3.5, 650, c("2Y","3Y","4Y","5Y"), col = c("black","blue","red","chartreuse3"), text.col = "black", lty = 1,merge = TRUE, bg = "gray90")

persp(strike_list*100, maturity_list, ans_hjm9$ive*100 ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Price")

persp(strike_list*100, maturity_list, ans_hjm9$price*10000 ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Price")

timestamp = Sys.time()
ans_hjm10 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"nag-niederreiter")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
ans_hjm11 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"rnorm")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
#maturity_list = c(2,2.5,3,3.5,4,4.5,5)
maturity_list = c(2,3)
strike_list = c(0.001,0.002,0.005,0.01,0.015,0.02)
ans_hjm12 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"sobol")
persp(strike_list*100, maturity_list, ans_hjm12$price ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Volatility")
cat("time to complete",Sys.time()-timestamp,"\n")

timestamp = Sys.time()
ans_hjm13 = HeathJarrowMortonPricing("cap",1,2,c(0.001,0.002,0.003,0.004,0.005,0.01),ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,nbsim_50000,"rnorm")
cat("time to complete",Sys.time()-timestamp,"\n")

# 
# ans_hjm3 = HeathJarrowMortonPricing("swap",1,c(2,3,4,5),NA,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,100,"nag-sobol")
# 
# 
# ans_hjm4 = HeathJarrowMortonPricing("swaption",1,maturity_list,strike_list,ValuationDateForwardCurve$rate/100,ValuationDateOISYieldCurve,100,"nag-sobol")
# 
# persp(strike_list*100, maturity_list, ans_hjm4$price ,phi = 10, theta = 45,r=5, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Price")

