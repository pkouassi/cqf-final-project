#multi-variate normal distribution
#install.packages("mnormt")
library(mnormt)



#test
plot(dnorm,xlim=c(-10,10))
plot(pnorm,xlim=c(-10,10))
plot(qnorm,xlim=c(0,1))
qnorm(0.99)

vasicekPDF = function(x,p,rho) sqrt((1-rho)/rho)*exp(-(1/(2*rho))*(sqrt(1-rho)*qnorm(x)-qnorm(p))^2+(1/2)*qnorm(x)^2)
vasicekPDFmode = function(p,rho) pnorm(sqrt(1-rho)/(1-2*rho)*qnorm(p))

vasicekCDF = function(x,p,rho) pnorm((sqrt(1-rho)*qnorm(x)-qnorm(p))/sqrt(rho))
X = seq(0,0.04,by=0.0001)
#percentile. percentile of 5% is the value under which we can 5% of the observation
vasicekInverseCDF = function(alpha,p,rho) vasicekCDF(alpha,1-p,1-rho)

#plot vasicek PDF - Probability density function
p = 0.01
rho = 0.04

Y_PDF = sapply(X,vasicekPDF,p=p,rho=rho)
plot(X,Y_PDF,type="l", main="Loss distribution - Probability density function",xlab="Probability",ylab="Density")

L_mode = vasicekPDFmode(p=p,rho=rho)
abline(v =L_mode, untf = FALSE, col = "gray60")
mean = p #mean = p
abline(v =mean, untf = FALSE, col = "orange")
#Var L = bivariate N(N-1(p),N-1(p),rho) - p^2
variance = pmnorm(c(qnorm(p),qnorm(p)),varcov=matrix(c(1,rho,rho,1),ncol=2,nrow=2,byrow=TRUE))-p^2
sigma= sqrt(variance)


alpha = 0.99
#(L_alpha-p)/sigma || L_alpha centered and scaled
value = (vasicekInverseCDF(alpha,p,rho)-p)/sqrt(variance)


#If the lender wishes
#to hold the probability of default on his notes at 1 - alpha = 0.001
#i.er the lender wish that only 0.1% (1-alpha) of the observations are above 


minus_sigma_level = mean - sqrt(variance)
plus_sigma_level = mean + sqrt(variance)
abline(v =minus_sigma_level, untf = FALSE, col = "green")
abline(v =plus_sigma_level, untf = FALSE, col = "purple")


#plot vasicek CDF
Y_CDF = sapply(X,vasicekCDF,p=0.01,rho=0.4)
par(mar=c(5,4,4,5)+.1)
plot(X,Y_PDF,type="l", main="Loss distribution - Probability density function",xlab="Probability",ylab="Density", col="red")
par(new=TRUE)
plot(X,Y_CDF,type="l",col="blue",main="",xlab="",ylab="")
axis(4)
mtext("y2",side=4,line=3)
legend("bottomright",col=c("red","blue"),lty=1,legend=c("Y_CDF","Y_PDF"))


#Sample test


# GCC_Fin = data.frame(
#   Rating = c("CCC","B","BB","BBB","A","AA","AAA"),
#   SurvivalProbability = c(NA,0.7339,0.8694,0.8977,0.9121,0.9658,0.9835), 
#   AttachementPoint = c(0,0.03,0.07,0.15,0.20,0.30,0.50),
#   DettachmentPoint = c(0.03,0.07,0.15,0.20,0.30,0.50,0.70)
# )  

GCC_Fin = data.frame(
  Rating = c("CCC","B","BB","BBB","A","AA","AAA"),
  SurvivalProbability = c(NA,0.7339,0.8694,0.8977,0.9121,0.9658,0.9835), 
  AttachementPoint = c(0,0.1,0.2,0.3,0.4,0.5,0.6),
  DettachmentPoint = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7)
) 

#------- tests
vasicekCDF(0.07,p,rho)

func = function(x) {
  #x[1]: p
  #x[2]: rho

  out = vasicekCDF(GCC_Fin$AttachementPoint[c(2,3,4,5,6,7)],x[1],x[2]) - GCC_Fin$SurvivalProbability[c(2,3,4,5,6,7)]
  return(out)
}

func2 = function(x) {
  #x[1]: p
  #x[2]: rho
  out = 0
  for (i in seq(2,7)) {
    out = out + (vasicekCDF(GCC_Fin$DettachmentPoint[i],x[1],x[2]) - GCC_Fin$SurvivalProbability[i])^2
  }
  return(out)
}

x0 = c(0.03,0.5)



lala = optim(par=x0, fn=func, gr = NULL,
      method = c("L-BFGS-B"),
      lower = c(0.000001,0.000001), upper = c(0.99999,0.99999),
      control = list(), hessian = F)

res = optim(par=x0, fn=func2, gr = NULL,
             method = c("L-BFGS-B"),
             lower = c(0.000001,0.000001), upper = c(0.99999,0.99999),
             control = list(), hessian = F)
est_p = res$par[1]
est_rho = res$par[2]

#estimation of CCC Survival Probability
vasicekCDF(0.1,est_p,est_rho)

#non linear least square
xdata = c(0.07,0.15,0.20,0.30,0.50,0.70)
ydata = c(0.7339,0.8694,0.8977,0.9121,0.9658,0.9835)

plot(xdata,ydata,type="l")
p1 = 0.03 #probability p
p2 = 0.5 # correlation rho






