#Sampling from gaussian copula

ans = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                  YieldCurve,CorrelationMatrix_GaussianCopula,0.40,10000)



objective_function = function(x) {
  res = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 2, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 3, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 4, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 5, marketprice = x)),0.40,yieldcurve_flat)
  return(res@hazardrate[5]-0.01)
}
uniroot(objective_function,lower=0,upper=10000)

#Risk and sensitivity analysis

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.00*x)))
cc_60 = GetFlatCreditCurve(60.370,yieldcurve_flat)
ans = BasketCDSPricing_GaussianCopula(c(cc_60,cc_60,cc_60,cc_60,cc_60),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,100000)
ans$basket_spreads
ans$singlename_spreads
ans$basket_sim
ans$singlename_sim
ans$tau

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







#Sampling from gaussian copula









yieldcurve_test = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))

cc = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = 60.301),
                            new ("CreditDefaultSwap", maturity = 2, marketprice = 60.301),
                            new ("CreditDefaultSwap", maturity = 3, marketprice = 60.301),
                            new ("CreditDefaultSwap", maturity = 4, marketprice = 60.301),
                            new ("CreditDefaultSwap", maturity = 5, marketprice = 60.301)),0.40,yieldcurve_test)

spread = 60
cc = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = spread),
                            new ("CreditDefaultSwap", maturity = 2, marketprice = spread),
                            new ("CreditDefaultSwap", maturity = 3, marketprice = spread),
                            new ("CreditDefaultSwap", maturity = 4, marketprice = spread),
                            new ("CreditDefaultSwap", maturity = 5, marketprice = spread)),0.40,yieldcurve_test)

corr_mat = UniformCorrelationMatrix(0.3,5)
BasketCDSPricing_GaussianCopula(cc,cc,cc,cc,cc,yieldcurve_test,UniformCorrelationMatrix(0.3,5),0.40,1,10000)

#**********************************************

FTDS_GaussianCopula(c(cc,cc,cc,cc,cc),yieldcurve_test,UniformCorrelationMatrix(0.3,5),0.40,10000)
FTDS_GaussianCopula(c(cc,cc,cc,cc,cc),yieldcurve_test,UniformCorrelationMatrix(0.0,5),0.40,300000)


spread_50 = 50
cc_50 = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 2, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 3, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 4, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 5, marketprice = spread_50)),0.40,yieldcurve_bbg)

lali = FTDS_GaussianCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_bbg,UniformCorrelationMatrix(0.6,5),0.40,100000)
lali$result

FTDS_GaussianCopula(c(cc_50),yieldcurve_test,UniformCorrelationMatrix(0.6,1),0.40,1000)


spread_350 = 350 
cc_350 = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 2, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 3, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 4, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 5, marketprice = spread_350)),0.40,yieldcurve_bbg)

ans3=FTDS_GaussianCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50,cc_350,cc_350,cc_350,cc_350,cc_350),yieldcurve_bbg,UniformCorrelationMatrix(0.0,10),0.40,10000)

############
#@Comparison with Bloomberg CDSN

yieldcurve_bbg = getYieldCurve(HistoricalYieldCurveMatrix,as.Date("25-APR-2014","%d-%b-%Y"))
GetFlatCreditCurve = function(x,yc) {
  return(BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 2, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 3, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 4, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 5, marketprice = x)),0.40,yc))
}


cc_IBM = GetFlatCreditCurve(40.06,yieldcurve_bbg)
cc_PFI = GetFlatCreditCurve(23.44,yieldcurve_bbg)
cc_DEL = GetFlatCreditCurve(244.88,yieldcurve_bbg)
cc_HPQ = GetFlatCreditCurve(22.00,yieldcurve_bbg)
cc_BMY = GetFlatCreditCurve(70.38,yieldcurve_bbg)

ans = FTDS_GaussianCopula(c(cc_IBM,cc_PFI,cc_DEL,cc_HPQ,cc_BMY),yieldcurve_bbg,UniformCorrelationMatrix(0.5,5),0.40,100000)
ans$result1; ans$result2

ans2 = BasketCDSPricing_GaussianCopulaV2(c(cc_IBM,cc_PFI,cc_DEL,cc_HPQ,cc_BMY),yieldcurve_bbg,UniformCorrelationMatrix(0.99,5),0.40,4,50000)
ans2$result1; ans2$result2

toto = ans$matsim

#--------------matching MPRA
# 60.301 ~ h = 0.01
#cc_1 = GetFlatCreditCurve(60.301,yieldcurve_bbg)
cc_1 = GetFlatCreditCurve(60.301,yieldcurve_bbg)

ans2 = BasketCDSPricing_GaussianCopulaV2(c(cc_1,cc_1,cc_1,cc_1,cc_1),yieldcurve_bbg,UniformCorrelationMatrix(0.3,5),0.40,1,1000000)
ans2$result1; ans2$result2

ans2 = BasketCDSPricing_GaussianCopulaV2(c(cc_1,cc_1,cc_1,cc_1,cc_1),yieldcurve_bbg,UniformCorrelationMatrix(0.3,5),0.40,2,1000000)
ans2$result1; ans2$result2

ans2 = BasketCDSPricing_GaussianCopulaV2(c(cc_1,cc_1,cc_1,cc_1,cc_1),yieldcurve_bbg,UniformCorrelationMatrix(0.3,5),0.40,3,1000000)
ans2$result1; ans2$result2

ans2 = BasketCDSPricing_GaussianCopulaV2(c(cc_1,cc_1,cc_1,cc_1,cc_1),yieldcurve_bbg,UniformCorrelationMatrix(0.3,5),0.40,4,1000000)
ans2$result1; ans2$result2


#############
#Retour au kth basket


cc_60 = GetFlatCreditCurve(60,yieldcurve_bbg)
ans4= BasketCDSPricing_GaussianCopulaV2(cc_60,cc_60,cc_60,cc_60,cc_60,yieldcurve_bbg,UniformCorrelationMatrix(sqrt(0.6),5),0.40,1,2000000)
ans4$result2

ans4= BasketCDSPricing_GaussianCopulaV2(cc_60,cc_60,cc_60,cc_60,cc_60,yieldcurve_bbg,UniformCorrelationMatrix(0.5,5),0.40,2,300000)
ans4$result2

cc_307 = GetFlatCreditCurve(307.6266,yieldcurve_bbg)
ans6= BasketCDSPricing_GaussianCopulaV2(c(cc_307,cc_307,cc_307,cc_307,cc_307),yieldcurve_bbg,UniformCorrelationMatrix(0.5,5),0.40,1,50000)
ans2$result1; ans$result2

#############
#Compare with xls

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.00*x)))
cc_60 = GetFlatCreditCurve(60.370,yieldcurve_flat)

BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = 60),
                       new ("CreditDefaultSwap", maturity = 2, marketprice = 60),
                       new ("CreditDefaultSwap", maturity = 3, marketprice = 60),
                       new ("CreditDefaultSwap", maturity = 4, marketprice = 60),
                       new ("CreditDefaultSwap", maturity = 5, marketprice = 60)),0.40,yieldcurve_flat)

ans = FTDS_GaussianCopula(c(cc_60,cc_60,cc_60),yieldcurve_bbg,UniformCorrelationMatrix(0.3,3),0.40,1000)
ans$result1; ans$result2