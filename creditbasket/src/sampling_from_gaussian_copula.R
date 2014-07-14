#==============================================================================
# title           :sampling_from_gaussian_copula.R
# description     :calls the pricing function for Gaussian Copula sampling
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

#==============================================================================
# Pricing results    
#==============================================================================

# Sampling from gaussian copula
# Pricing with our basket
# 10,000 simulations (can be increased by changing parameter)
# Use Sobol from R package "randtoolbox"
ans1 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                  YieldCurve,CorrelationMatrix_GaussianCopula,0.40,10000,"sobol")


# Plot first to default - convergence diagram
plot(seq(100,10000),ans1$basket_sim[100:10000,1]/ans1$basket_sim[100:10000,6],type="l",log="x")

# Plot second to default - convergence diagram
plot(seq(100,10000),ans1$basket_sim[100:10000,2]/ans1$basket_sim[100:10000,7],type="l",log="x")

# Plot third to default
plot(seq(100,10000),ans1$basket_sim[100:10000,3]/ans1$basket_sim[100:10000,8],type="l",log="x")

# Use Halton from R package "randtoolbox"
ans2 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                         CDS2_USD_XR_MARGINAL_CreditCurve,
                                         CDS3_USD_XR_MARGINAL_CreditCurve,
                                         CDS4_USD_XR_MARGINAL_CreditCurve,
                                         CDS5_USD_XR_MARGINAL_CreditCurve),
                                       YieldCurve,CorrelationMatrix_GaussianCopula,0.40,10000,"halton")

# Use pseudo-random numbers (rnorm())
ans3 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                         CDS2_USD_XR_MARGINAL_CreditCurve,
                                         CDS3_USD_XR_MARGINAL_CreditCurve,
                                         CDS4_USD_XR_MARGINAL_CreditCurve,
                                         CDS5_USD_XR_MARGINAL_CreditCurve),
                                       YieldCurve,CorrelationMatrix_GaussianCopula,0.40,10000,"rnorm")
# Use Niederreiter
# requires NAG Fortran 64 bit
# BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
#                                   CDS2_USD_XR_MARGINAL_CreditCurve,
#                                   CDS3_USD_XR_MARGINAL_CreditCurve,
#                                   CDS4_USD_XR_MARGINAL_CreditCurve,
#                                   CDS5_USD_XR_MARGINAL_CreditCurve),
#                                 YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-niederreiter")

#==============================================================================
# Risk and sensitivity analysis  
#==============================================================================

# Example - Correlation sensitivity analysis
yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans4 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.0,5),0.40,10000,"sobol")
ans5 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.1,5),0.40,10000,"sobol")
ans6 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.2,5),0.40,10000,"sobol")
ans7 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,10000,"sobol")
ans8 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.4,5),0.40,10000,"sobol")
ans9 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.5,5),0.40,10000,"sobol")
ans10 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.6,5),0.40,10000,"sobol")
ans11 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.7,5),0.40,10000,"sobol")
ans12 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.8,5),0.40,10000,"sobol")
ans13 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.9,5),0.40,10000,"sobol")
ans14 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.99,5),0.40,10000,"sobol")

X = c(0,10,20,30,40,50,60,70,80,90,99)
Y = matrix(NA,nrow=5,ncol=length(X))
Y[,1] = ans4$basket_spreads
Y[,2] = ans5$basket_spreads
Y[,3] = ans6$basket_spreads
Y[,4] = ans7$basket_spreads
Y[,5] = ans8$basket_spreads
Y[,6] = ans9$basket_spreads
Y[,7] = ans10$basket_spreads
Y[,8] = ans11$basket_spreads
Y[,9] = ans12$basket_spreads
Y[,10] = ans13$basket_spreads
Y[,11] = ans14$basket_spreads

matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Equicorrelation (%)", col="black", lty = c(1,2,3), pch = c(1,2,3))
legend(60, 100, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"), text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),merge = TRUE, bg = "gray90")

# Example - Recovery rate analysis
yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans15 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.30,10000,"sobol")
ans16 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,10000,"sobol")
ans17 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.50,10000,"sobol")
ans18 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.60,10000,"sobol")
ans19 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.70,10000,"sobol")

X = c(30,40,50,60,70)
Y = matrix(NA,nrow=5,ncol=length(X))
Y[,1] = ans15$basket_spreads
Y[,2] = ans16$basket_spreads
Y[,3] = ans17$basket_spreads
Y[,4] = ans18$basket_spreads
Y[,5] = ans19$basket_spreads

matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Equicorrelation (%)", col="black", lty = c(1,2,3), pch = c(1,2,3))
legend(58, 95, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"), text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),merge = TRUE, bg = "gray90")

