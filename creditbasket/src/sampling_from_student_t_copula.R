#Sampling from student t copula

ans_student1 = BasketCDSPricing_StudentTCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                                  YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,1000000,"nag-sobol")


#ans$basket_spreads
#ans$singlename_spreads
#ans$basket_sim
#ans$singlename_sim
#ans$tau

#first to default
#plot(seq(100,100000),ans$basket_sim[100:100000,1]/ans$basket_sim[100:100000,6],type="l",log="x")

#second to default
#plot(seq(100,100000),ans$basket_sim[100:100000,2]/ans$basket_sim[100:100000,7],type="l",log="x")

#third to default
#plot(seq(100,100000),ans$basket_sim[100:100000,3]/ans$basket_sim[100:100000,8],type="l",log="x")

#rnorm vs. sobol
#first_to_default.rnorm = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#first_to_default.sobol = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#matplot(seq(250,100000),cbind(first_to_default.rnorm,first_to_default.sobol),type="l",log="x")


#Risk and sensitivity analysis

#yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.00*x)))
#cc_60 = GetFlatCreditCurve(60.370,yieldcurve_flat)
#ans = BasketCDSPricing_StudentTCopula(c(cc_60,cc_60,cc_60,cc_60,cc_60),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),7,0.40,100000)
#ans$basket_spreads
#ans$singlename_spreads
#ans$basket_sim
#ans$singlename_sim
#ans$tau



yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
deg_freedom = 10

cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans_student2 = BasketCDSPricing_StudentTCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")

cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans_student3 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")

cc_250 = GetFlatCreditCurve(250,yieldcurve_flat)
ans_student4 = BasketCDSPricing_StudentTCopula(c(cc_250,cc_250,cc_250,cc_250,cc_250),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")

cc_500 = GetFlatCreditCurve(500,yieldcurve_flat)
ans_student5 = BasketCDSPricing_StudentTCopula(c(cc_500,cc_500,cc_500,cc_500,cc_500),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")
  

ans_student6 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.0,5),deg_freedom,0.40,1000000,"nag-sobol")
ans_student7 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")
ans_student8 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.6,5),deg_freedom,0.40,1000000,"nag-sobol")
ans_student9 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.99,5),deg_freedom,0.40,1000000,"nag-sobol")

ans_student10 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.30,1000000,"nag-sobol")
ans_student11 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.50,1000000,"nag-sobol")
ans_student12 = BasketCDSPricing_StudentTCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.60,1000000,"nag-sobol")

#yield curve
yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans_student13 = BasketCDSPricing_StudentTCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.02*x)))
cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans_student14 = BasketCDSPricing_StudentTCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.03*x)))
cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans_student15 = BasketCDSPricing_StudentTCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.05*x)))
cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans_student16 = BasketCDSPricing_StudentTCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),deg_freedom,0.40,1000000,"nag-sobol")


# X = c(1,2,4,5)
# Y = matrix(NA,nrow=5,ncol=4)
# Y[,1] = ans_student13$basket_spreads
# Y[,2] = ans_student14$basket_spreads
# Y[,3] = ans_student15$basket_spreads
# Y[,4] = ans_student16$basket_spreads
# matplot(X,t(Y)[,1],type="b",ylab="Basket spread (bp)",xlab="Recovery rate (%)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5))
# 

#ans_student13$basket_spreads
#ans_student14$basket_spreads
#ans_student15$basket_spreads
#ans_student16$basket_spreads

