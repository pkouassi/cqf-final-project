#==============================================================================
# title           :sampling_from_student_t_copula.R
# description     :calls the pricing function for Student's t Copula sampling
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

#==============================================================================
# Pricing results    
#==============================================================================

# Sampling from student's t copula
# Pricing with our basket
# 10,000 simulations (can be increased by changing parameter)
# Use Sobol from R package "randtoolbox"
ans_student1 = BasketCDSPricing_StudentTCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                                  YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,10000,"sobol")

# Plot first to default - convergence diagram
plot(seq(100,10000),ans_student1$basket_sim[100:10000,1]/ans_student1$basket_sim[100:10000,6],type="l",log="x")

# Plot second to default - convergence diagram
plot(seq(100,10000),ans_student1$basket_sim[100:10000,2]/ans_student1$basket_sim[100:10000,7],type="l",log="x")

# Plot third to default
plot(seq(100,10000),ans_student1$basket_sim[100:10000,3]/ans_student1$basket_sim[100:10000,8],type="l",log="x")

# Use Halton from R package "randtoolbox"
ans_student2 = BasketCDSPricing_StudentTCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                                 CDS2_USD_XR_MARGINAL_CreditCurve,
                                                 CDS3_USD_XR_MARGINAL_CreditCurve,
                                                 CDS4_USD_XR_MARGINAL_CreditCurve,
                                                 CDS5_USD_XR_MARGINAL_CreditCurve),
                                               YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,10000,"halton")

# Use pseudo-random numbers (rnorm())
ans_student3 = BasketCDSPricing_StudentTCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                                 CDS2_USD_XR_MARGINAL_CreditCurve,
                                                 CDS3_USD_XR_MARGINAL_CreditCurve,
                                                 CDS4_USD_XR_MARGINAL_CreditCurve,
                                                 CDS5_USD_XR_MARGINAL_CreditCurve),
                                               YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,10000,"rnorm")
# Use Niederreiter
# requires NAG Fortran 64 bit
# ans_student4 = BasketCDSPricing_StudentTCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
#                                                  CDS2_USD_XR_MARGINAL_CreditCurve,
#                                                  CDS3_USD_XR_MARGINAL_CreditCurve,
#                                                  CDS4_USD_XR_MARGINAL_CreditCurve,
#                                                  CDS5_USD_XR_MARGINAL_CreditCurve),
#                                                YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,10000,"niederreiter")

#==============================================================================
# Risk and sensitivity analysis  
#==============================================================================

# Example - Correlation sensitivity analysis
yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans_student4 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.0,5),degree_freedom,0.40,10000,"sobol")
ans_student5 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.1,5),degree_freedom,0.40,10000,"sobol")
ans_student6 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.2,5),degree_freedom,0.40,10000,"sobol")
ans_student7 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),degree_freedom,0.40,10000,"sobol")
ans_student8 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.4,5),degree_freedom,0.40,10000,"sobol")
ans_student9 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.5,5),degree_freedom,0.40,10000,"sobol")
ans_student10 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.6,5),degree_freedom,0.40,10000,"sobol")
ans_student11 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.7,5),degree_freedom,0.40,10000,"sobol")
ans_student12 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.8,5),degree_freedom,0.40,10000,"sobol")
ans_student13 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.9,5),degree_freedom,0.40,10000,"sobol")
ans_student14 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.99,5),degree_freedom,0.40,10000,"sobol")

X = c(0,10,20,30,40,50,60,70,80,90,99)
Y = matrix(NA,nrow=5,ncol=length(X))
Y[,1] = ans_student4$basket_spreads
Y[,2] = ans_student5$basket_spreads
Y[,3] = ans_student6$basket_spreads
Y[,4] = ans_student7$basket_spreads
Y[,5] = ans_student8$basket_spreads
Y[,6] = ans_student9$basket_spreads
Y[,7] = ans_student10$basket_spreads
Y[,8] = ans_student11$basket_spreads
Y[,9] = ans_student12$basket_spreads
Y[,10] = ans_student13$basket_spreads
Y[,11] = ans_student14$basket_spreads

matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Equicorrelation (%)", col="black", lty = c(1,2,3), pch = c(1,2,3))
legend(60, 100, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"), text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),merge = TRUE, bg = "gray90")

# Example - Recovery rate analysis
yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans_student15 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),degree_freedom,0.30,10000,"sobol")
ans_student16 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),degree_freedom,0.40,10000,"sobol")
ans_student17 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),degree_freedom,0.50,10000,"sobol")
ans_student18 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),degree_freedom,0.60,10000,"sobol")
ans_student19 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),degree_freedom,0.70,10000,"sobol")

X = c(30,40,50,60,70)
Y = matrix(NA,nrow=5,ncol=length(X))
Y[,1] = ans_student15$basket_spreads
Y[,2] = ans_student16$basket_spreads
Y[,3] = ans_student17$basket_spreads
Y[,4] = ans_student18$basket_spreads
Y[,5] = ans_student19$basket_spreads

matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Equicorrelation (%)", col="black", lty = c(1,2,3), pch = c(1,2,3))
legend(58, 95, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"), text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),merge = TRUE, bg = "gray90")


