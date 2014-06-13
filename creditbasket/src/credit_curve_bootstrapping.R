#Credit Curve Bootstrapping

#class definition
setClass("CreditDefaultSwap", 
         representation(
           maturity = "numeric", 
           marketprice = "numeric" #market spread
         )
)

setClass("YieldCurve",
         representation(
           time = "vector",
           discountfactor = "vector")
)

setClass("CreditCurve",
         representation(
           time = "vector",
           spread = "vector",
           survivalprobability = "vector",
           hazardrate = "vector")
)

#function definition
GetSurvivalProbability = function(CreditCurve,t) {
  result = NA
  t_index = match(t,CreditCurve@time)
  if (!is.na(CreditCurve@survivalprobability[t_index])) {
    result = CreditCurve@survivalprobability[t_index]
  }
  return (result)
}

GetDiscountFactor = function(YieldCurve,t) {
  min_time = min(YieldCurve@time)
  min_time_index = which.min(YieldCurve@time)
  max_time = max(YieldCurve@time)
  max_time_index = which.max(YieldCurve@time)
  
  result = NA
  if (t < 0) {
    cat("Warning: t is negative. discountfactor not calculated for this case")
  }
  else if (t == 0) {
    result = 1 #df of t=0 is 1
  }
  else if (t>0 && t<min_time) {
    #df of t=0 is 1
    result = 1 + (YieldCurve@discountfactor[min_time_index]-1)*(t/min_time)
  }
  else if (t >= max_time) {
    result = YieldCurve@discountfactor[max_time_index]
  }
  else {
    #i.e t falls between 2 maturity for which we have the discount factor
    for (i in seq(1,length(YieldCurve@time)-1)) {
      if (t>= YieldCurve@time[i] && t<YieldCurve@time[i+1]) {
        result = YieldCurve@discountfactor[i] + (YieldCurve@discountfactor[i+1]-YieldCurve@discountfactor[i])*((t-YieldCurve@time[i])/(YieldCurve@time[i+1]-YieldCurve@time[i]))
      }                                                                                                                 
    }
  }
  return (result)  
}



BootstrapCreditCurve = function (CDSCollection,RecoveryRate,YieldCurve) {
  #Credit Curve initialization
  CreditCurve = new ("CreditCurve", time = rep(NA,length(CDSCollection)), survivalprobability = rep(NA,length(CDSCollection)), hazardrate=rep(NA,length(CDSCollection)))
  
  for (i in seq(1,length(CDSCollection))) {
    CreditCurve@time[i] = CDSCollection[[i]]@maturity
    CreditCurve@spread[i] = CDSCollection[[i]]@marketprice
  }
  
  #first maturity pillar   
  CreditCurve@survivalprobability[1] = (1-RecoveryRate)/((1-RecoveryRate)+CreditCurve@time[1]*(CreditCurve@spread[1]/10000))
  CreditCurve@hazardrate[1] = (-1*log(CreditCurve@survivalprobability[1]))/CreditCurve@time[1]
  
  #subsequent maturity pillars
  if (length(CDSCollection)>1) {
    for (i in seq(2,length(CDSCollection))) {
      last_term = GetSurvivalProbability(CreditCurve,CreditCurve@time[i-1]) * (1-RecoveryRate) / (1-RecoveryRate + (CreditCurve@time[i]-CreditCurve@time[i-1]) * (CreditCurve@spread[i]/10000))
      
      #assumption is that we have a CDS for each year
      #loop through every CDS until the year before the maturity of current CDS (ith CDS)
      first_term = 0
      for (j in seq(1,i-1)) {
        if (j==1) {
          #specific case for t=0. survival probability of t=0 is 1
          term = GetDiscountFactor(YieldCurve,CreditCurve@time[j]) * ((1-RecoveryRate) * 1 - (1-RecoveryRate + (CreditCurve@time[j]-0) * (CreditCurve@spread[i]/10000)) * GetSurvivalProbability(CreditCurve,CreditCurve@time[j]))
        }
        else {
          term = GetDiscountFactor(YieldCurve,CreditCurve@time[j]) * ((1-RecoveryRate) * GetSurvivalProbability(CreditCurve,CreditCurve@time[j-1]) - (1-RecoveryRate + (CreditCurve@time[j]-CreditCurve@time[j-1]) * (CreditCurve@spread[i]/10000)) * GetSurvivalProbability(CreditCurve,CreditCurve@time[j]))
        }
        first_term = first_term + term
      }
      
      quotient = (GetDiscountFactor(YieldCurve,CreditCurve@time[i]) * (1-RecoveryRate + (CreditCurve@time[i]-CreditCurve@time[i-1]) * (CreditCurve@spread[i]/10000)))
      first_term = first_term / quotient

      #assign survival probability and hazard rate
      CreditCurve@survivalprobability[i] = first_term + last_term  
      CreditCurve@hazardrate[i] = (-1/1)*log(CreditCurve@survivalprobability[i]/CreditCurve@survivalprobability[i-1])

    }
  }
  
  #DisplayCreditCurve = as.data.frame(matrix(ncol=5, nrow=length(CDSCollection)))
  #names(DisplayCreditCurve) = c("Time", "MarketSpread","DF", "SurvivalProbability","HazardRate")
  #for (i in seq(1,length(CDSCollection)) ) {
  #  DisplayCreditCurve$Time[i] = CreditCurve@time[i] 
  #  DisplayCreditCurve$MarketSpread[i] = CreditCurve@spread[i] 
  #  DisplayCreditCurve$DF[i] = GetDiscountFactor(YieldCurve,CreditCurve@time[i]) 
  #  DisplayCreditCurve$SurvivalProbability[i] = CreditCurve@survivalprobability[i] 
  #  DisplayCreditCurve$HazardRate[i] = CreditCurve@hazardrate[i] 
  #}  
  #print(DisplayCreditCurve)
  
  return(CreditCurve)
}

######################################
#Numerical examples
######################################
CDSMarketData = as.data.frame(matrix(ncol=4, nrow=5))
names(CDSMarketData) = c("Maturity", "WFC","CCMO", "Z")
CDSMarketData[1,] = c(1,50,751,0.97)
CDSMarketData[2,] = c(2,77,1164,0.94)
CDSMarketData[3,] = c(3,94,1874,0.92)
CDSMarketData[4,] = c(5,125,4156,0.86)
CDSMarketData[5,] = c(7,133,6083,0.81)

#interpolation of interest rates
plot_x_lim = c(0,7);
plot_y_lim = c(0.8,1);
plot(CDSMarketData$Maturity,CDSMarketData$Z,type="b",xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",main="Interpolation of Z(t,T)",xlab="Time",ylab="Discount Factor",pch=4)
par(new=TRUE);
plot(c(4,6),c(0.5*(CDSMarketData$Z[3]+CDSMarketData$Z[4]),0.5*(CDSMarketData$Z[4]+CDSMarketData$Z[5])),type="p",xlim=plot_x_lim, ylim=plot_y_lim,col ="red",main="",xlab="",ylab="",pch=2,lwd=2)
legend(3, 1.0, c("Market Data","Interpolated Data"), col = c("blue","red"),
       text.col = "black", lty = c(1,NA), pch = c(4,2),
       merge = TRUE, bg = "gray90")
#interpolation of credit
plot_x_lim = c(1,7);
plot_y_lim = c(40,133);
plot(CDSMarketData$Maturity,CDSMarketData$WFC,type="b",xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",main="Interpolation of CDS Spread (WFC)",xlab="Time",ylab="CDS Spread",pch=4)
par(new=TRUE);
plot(c(4,6),c(0.5*(CDSMarketData$WFC[3]+CDSMarketData$WFC[4]),0.5*(CDSMarketData$WFC[4]+CDSMarketData$WFC[5])),type="p",xlim=plot_x_lim, ylim=plot_y_lim,col ="red",main="",xlab="",ylab="",pch=2,lwd=2)
par(new=TRUE);
plot(c(4,6),c(sqrt(CDSMarketData$WFC[3]*CDSMarketData$WFC[4]),sqrt(CDSMarketData$WFC[4]*CDSMarketData$WFC[5])),type="p",xlim=plot_x_lim, ylim=plot_y_lim,col ="chartreuse4",main="",xlab="",ylab="",pch=9,lwd=2)
legend(1.8, 62, c("Market Data","Interpolated Data (aritm avg)","Interpolated Data (geom avg)"), col = c("blue","red","chartreuse4"),
       text.col = "black", lty = c(1,NA,NA), pch = c(4,2,9),
       merge = TRUE, bg = "gray90")

plot_x_lim = c(1,7);
plot_y_lim = c(750,6100);
plot(CDSMarketData$Maturity,CDSMarketData$CCMO,type="b",xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",main="Interpolation of CDS Spread (CCMO)",xlab="Time",ylab="CDS Spread",pch=4)
par(new=TRUE);
plot(c(4,6),c(0.5*(CDSMarketData$CCMO[3]+CDSMarketData$CCMO[4]),0.5*(CDSMarketData$CCMO[4]+CDSMarketData$CCMO[5])),type="p",xlim=plot_x_lim, ylim=plot_y_lim,col ="red",main="",xlab="",ylab="",pch=2,lwd=2)
par(new=TRUE);
plot(c(4,6),c(sqrt(CDSMarketData$CCMO[3]*CDSMarketData$CCMO[4]),sqrt(CDSMarketData$CCMO[4]*CDSMarketData$CCMO[5])),type="p",xlim=plot_x_lim, ylim=plot_y_lim,col ="chartreuse4",main="",xlab="",ylab="",pch=9,lwd=2)
legend(1, 6000, c("Market Data","Interp. Data (aritm avg)","Interp. Data (geom avg)"), col = c("blue","red","chartreuse4"),
       text.col = "black", lty = c(1,NA,NA), pch = c(4,2,9),
       merge = TRUE, bg = "gray90")

######################################
options(digits=15) 

CDS1Y_TEST = new ("CreditDefaultSwap", maturity = 1, marketprice = 29.00)
CDS2Y_TEST = new ("CreditDefaultSwap", maturity = 2, marketprice = 39.00)
CDS3Y_TEST = new ("CreditDefaultSwap", maturity = 3, marketprice = 46.00)
CDS4Y_TEST = new ("CreditDefaultSwap", maturity = 4, marketprice = 52.00)
CDS5Y_TEST = new ("CreditDefaultSwap", maturity = 5, marketprice = 57.00)
CDSCollection_TEST = c(CDS1Y_TEST,CDS2Y_TEST,CDS3Y_TEST,CDS4Y_TEST,CDS5Y_TEST)
RecoveryRate_TEST = 0.50
Yieldcurve = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = c(0.9803,0.9514,0.9159,0.8756,0.8328))
print("TEST - spread term structure")
BootstrapCreditCurve(CDSCollection_TEST,RecoveryRate_TEST,Yieldcurve)

######################################
WFC - Wells Fargo
######################################
RecoveryRate_WFC = 0.50
Yieldcurve = new ("YieldCurve", time = c(1,2,3,5,7), discountfactor = c(0.97,0.94,0.92,0.86,0.81))

CDS1Y_WFC = new ("CreditDefaultSwap", maturity = 1, marketprice = 29.00)
CDS2Y_WFC = new ("CreditDefaultSwap", maturity = 2, marketprice = 39.00)
CDS3Y_WFC = new ("CreditDefaultSwap", maturity = 3, marketprice = 46.00)
CDS5Y_WFC = new ("CreditDefaultSwap", maturity = 5, marketprice = 52.00)
CDS7Y_WFC = new ("CreditDefaultSwap", maturity = 7, marketprice = 57.00)

#arithmetic averaging
CDS4Y_WFC_arithmetic_average = new ("CreditDefaultSwap", maturity = 4, marketprice = (46.00+52.00)/2)
CDS6Y_WFC_arithmetic_average = new ("CreditDefaultSwap", maturity = 6, marketprice = (52.00+57.00)/2)
CDSCollection_WFC_arithmetic_average = c(CDS1Y_WFC,CDS2Y_WFC,CDS3Y_WFC,CDS4Y_WFC_arithmetic_average,CDS5Y_WFC,CDS6Y_WFC_arithmetic_average,CDS7Y_WFC)
CreditCurve_WFC_arithmetic_average = BootstrapCreditCurve(CDSCollection_WFC_arithmetic_average,RecoveryRate_WFC,Yieldcurve)

#geometric averaging
CDS4Y_WFC_geometric_average = new ("CreditDefaultSwap", maturity = 4, marketprice = sqrt(46.00*52.00))
CDS6Y_WFC_geometric_average = new ("CreditDefaultSwap", maturity = 6, marketprice = sqrt(52.00*57.00))
CDSCollection_WFC_geometric_average = c(CDS1Y_WFC,CDS2Y_WFC,CDS3Y_WFC,CDS4Y_WFC_geometric_average,CDS5Y_WFC,CDS6Y_WFC_geometric_average,CDS7Y_WFC)
CreditCurve_WFC_geometric_average = BootstrapCreditCurve(CDSCollection_WFC_geometric_average,RecoveryRate_WFC,Yieldcurve)

#plot survival probability - comparison of interpolation method: 
#arithmetic average of CDS spread of geometric average of CDS spread
plot_x_lim = c(0,8);
plot_y_lim = c(0.92,1);
plot(CreditCurve_WFC_arithmetic_average@time,CreditCurve_WFC_arithmetic_average@survivalprobability,type = "l",xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",main="Interpolation\n(arithmetic avg vs. gometric avg)",xlab="Time",ylab="Survival Probability")
par(new=TRUE);
plot(CreditCurve_WFC_geometric_average@time,CreditCurve_WFC_geometric_average@survivalprobability,type = "l",xlim=plot_x_lim, ylim=plot_y_lim,col ="red",main="",xlab="",ylab="",lty=2, lwd=2)
legend(4, 1.0, c("Arithmetic","Geometric"), col = c("blue","red"),
       text.col = "black", lty = c(1,2), pch = c(NA,NA),
       merge = TRUE, bg = "gray90")


#plot hazard rate - comparison of interpolation method: 
#arithmetic average of CDS spread of geometric average of CDS spread
barplot(rbind(CreditCurve_WFC_arithmetic_average@hazardrate,CreditCurve_WFC_geometric_average@hazardrate), axes=FALSE, axisnames = FALSE, main="Interpolation\n(arithmetic avg vs. gometric avg)",
        xlab="Time", ylab="Hazard Rate", col=c("darkblue","red"),beside=TRUE, legend.text = c("Arithmetic", "Geometric"),args.legend = list(x=5, y=0.015, bty = "n"))
axis(1,at=c(2,5,8,11,14,17,20),labels=CreditCurve_WFC_arithmetic_average@time)
axis(2,at=seq(0,0150,by=0.0010),labels=seq(0,0150,by=0.0010))


######################################
CCMO - Clear Channel Communications
######################################
RecoveryRate_CCMO = 0.10
Yieldcurve = new ("YieldCurve", time = c(1,2,3,5,7), discountfactor = c(0.97,0.94,0.92,0.86,0.81))

CDS1Y_CCMO = new ("CreditDefaultSwap", maturity = 1, marketprice = 751.00)
CDS2Y_CCMO = new ("CreditDefaultSwap", maturity = 2, marketprice = 1164.00)
CDS3Y_CCMO = new ("CreditDefaultSwap", maturity = 3, marketprice = 1874.00)
CDS5Y_CCMO = new ("CreditDefaultSwap", maturity = 5, marketprice = 4156.00)
CDS7Y_CCMO = new ("CreditDefaultSwap", maturity = 7, marketprice = 6083.00)

#arithmetic averaging
CDS4Y_CCMO_arithmetic_average = new ("CreditDefaultSwap", maturity = 4, marketprice = (1874.00+4156.00)/2)
CDS6Y_CCMO_arithmetic_average = new ("CreditDefaultSwap", maturity = 6, marketprice = (4156.00+6083.00)/2)
CDSCollection_CCMO_arithmetic_average = c(CDS1Y_CCMO,CDS2Y_CCMO,CDS3Y_CCMO,CDS4Y_CCMO_arithmetic_average,CDS5Y_CCMO,CDS6Y_CCMO_arithmetic_average,CDS7Y_CCMO)
CreditCurve_CCMO_arithmetic_average = BootstrapCreditCurve(CDSCollection_CCMO_arithmetic_average,RecoveryRate_CCMO,Yieldcurve)

#geometric averaging
CDS4Y_CCMO_geometric_average = new ("CreditDefaultSwap", maturity = 4, marketprice = sqrt(1874.00*4156.00))
CDS6Y_CCMO_geometric_average = new ("CreditDefaultSwap", maturity = 6, marketprice = sqrt(4156.00*6083.00))
CDSCollection_CCMO_geometric_average = c(CDS1Y_CCMO,CDS2Y_CCMO,CDS3Y_CCMO,CDS4Y_CCMO_geometric_average,CDS5Y_CCMO,CDS6Y_CCMO_geometric_average,CDS7Y_CCMO)
CreditCurve_CCMO_geometric_average = BootstrapCreditCurve(CDSCollection_CCMO_geometric_average,RecoveryRate_CCMO,Yieldcurve)

#plot survival probability - comparison of interpolation method: 
#arithmetic average of CDS spread of geometric average of CDS spread
plot_x_lim = c(1,7);
plot_y_lim = c(-.55,1);
plot(CreditCurve_CCMO_arithmetic_average@time,CreditCurve_CCMO_arithmetic_average@survivalprobability,type = "l",xlim=plot_x_lim, ylim=plot_y_lim,col ="blue",main="Interpolation\n(arithmetic avg vs. gometric avg)",xlab="Time",ylab="Survival Probability")
par(new=TRUE);
plot(CreditCurve_CCMO_geometric_average@time,CreditCurve_CCMO_geometric_average@survivalprobability,type = "l",xlim=plot_x_lim, ylim=plot_y_lim,col ="red",main="",xlab="",ylab="",lty=2, lwd=2)
legend(5, 1.0, c("Arithmetic","Geometric"), col = c("blue","red"),
       text.col = "black", lty = c(1,2), pch = c(NA,NA),
       merge = TRUE, bg = "gray90")

#plot hazard rate - comparison of interpolation method: 
#arithmetic average of CDS spread of geometric average of CDS spread
barplot(rbind(CreditCurve_CCMO_arithmetic_average@hazardrate,CreditCurve_CCMO_geometric_average@hazardrate), axes=FALSE, axisnames = FALSE,main="Interpolation\n(arithmetic avg vs. gometric avg)",
        xlab="Time", ylab="Hazard Rate", col=c("darkblue","red"),beside=TRUE, legend.text = c("Arithmetic", "Geometric"),args.legend = list(x=10, y=-0.5, bty = "n") )
axis(1,at=c(2,5,8,11,14,17,20),labels=CreditCurve_CCMO_arithmetic_average@time)
axis(2,at=seq(-0.70,90,by=0.10),labels=seq(-0.70,90,by=0.10))

#Effect of increase of recovery rate on implied survival probabilities
RR = seq(0.1,0.9,by=0.1) #10%,20% .... 100%
RR_color = c("blue4","brown1","burlywood","darkred","blue","cornsilk4","deepskyblue3","deeppink","goldenrod4")
plot_x_lim = c(0,8);
plot_y_lim = c(0.6,1);
for (i in seq(1,length(RR))) {
  #bootstrap the credit curve
  tmp = BootstrapCreditCurve(CDSCollection_WFC_arithmetic_average,RR[i],Yieldcurve)
  #plot survival probabilities curve
  plot(tmp@time,tmp@survivalprobability,type = "l",xlim=plot_x_lim, ylim=plot_y_lim,col =RR_color[i],main="Survival Probabilities for various Recovery rates",xlab="Time",ylab="Survival Probability",lwd=1)
  par(new=TRUE);  
}
legend(0, 0.85, RR, col = RR_color,
       text.col = "black", lty = 1, lwd=2, pch = c(NA,NA),
       merge = TRUE, bg = "gray90")


