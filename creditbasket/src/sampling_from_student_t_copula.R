#Sampling from student t copula

ans = BasketCDSPricing_StudentTCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                                  YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,100000)

#ans$basket_spreads
#ans$singlename_spreads
#ans$basket_sim
#ans$singlename_sim
#ans$tau

#first to default
plot(seq(100,100000),ans$basket_sim[100:100000,1]/ans$basket_sim[100:100000,6],type="l",log="x")

#second to default
plot(seq(100,100000),ans$basket_sim[100:100000,2]/ans$basket_sim[100:100000,7],type="l",log="x")

#third to default
plot(seq(100,100000),ans$basket_sim[100:100000,3]/ans$basket_sim[100:100000,8],type="l",log="x")

#rnorm vs. sobol
#first_to_default.rnorm = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#first_to_default.sobol = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#matplot(seq(250,100000),cbind(first_to_default.rnorm,first_to_default.sobol),type="l",log="x")


#Risk and sensitivity analysis

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.00*x)))
cc_60 = GetFlatCreditCurve(60.370,yieldcurve_flat)
ans = BasketCDSPricing_StudentTCopula(c(cc_60,cc_60,cc_60,cc_60,cc_60),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),7,0.40,100000)
#ans$basket_spreads
#ans$singlename_spreads
#ans$basket_sim
#ans$singlename_sim
#ans$tau