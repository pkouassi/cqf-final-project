#Sampling from gaussian copula

ans1 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                  YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-sobol")


#first to default
#plot(seq(100,100000),ans$basket_sim[100:100000,1]/ans$basket_sim[100:100000,6],type="l",log="x")

#second to default
#plot(seq(100,100000),ans$basket_sim[100:100000,2]/ans$basket_sim[100:100000,7],type="l",log="x")

#third to default
#plot(seq(100,100000),ans$basket_sim[100:100000,3]/ans$basket_sim[100:100000,8],type="l",log="x")


#Risk and sensitivity analysis

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans2 = BasketCDSPricing_GaussianCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans3 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

cc_250 = GetFlatCreditCurve(250,yieldcurve_flat)
ans4 = BasketCDSPricing_GaussianCopula(c(cc_250,cc_250,cc_250,cc_250,cc_250),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

cc_500 = GetFlatCreditCurve(500,yieldcurve_flat)
ans5 = BasketCDSPricing_GaussianCopula(c(cc_500,cc_500,cc_500,cc_500,cc_500),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

ans6 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.0,5),0.40,1000000,"nag-sobol")
ans7 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.1,5),0.40,1000000,"nag-sobol")
ans8 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.2,5),0.40,1000000,"nag-sobol")
ans9 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")
ans10 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.4,5),0.40,1000000,"nag-sobol")
ans11 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.5,5),0.40,1000000,"nag-sobol")
ans12 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.6,5),0.40,1000000,"nag-sobol")
ans13 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.7,5),0.40,1000000,"nag-sobol")
ans14 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.8,5),0.40,1000000,"nag-sobol")
ans15 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.9,5),0.40,1000000,"nag-sobol")
ans16 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.99,5),0.40,1000000,"nag-sobol")

#to be re-run
ans17 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.30,1000000,"nag-sobol")
ans18 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.50,1000000,"nag-sobol")
ans19 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.60,1000000,"nag-sobol")
#-------------

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans20 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.02*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans21 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.03*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans22 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.05*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans23 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

#to be re-run
#sobol / convergence
ans24 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-sobol")

ans25 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                          YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-niederreiter")

ans26 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-faure")

ans27 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"rnorm")



#ans$basket_spreads
#ans$singlename_spreads
#ans$basket_sim
#ans$singlename_sim
#ans$tau

#rnorm vs. sobol
#first_to_default.rnorm = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#first_to_default.sobol = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#matplot(seq(250,100000),cbind(first_to_default.rnorm,first_to_default.sobol),type="l",log="x")




