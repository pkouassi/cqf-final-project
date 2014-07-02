#Sampling from gaussian copula
spread_gaussian = BasketCDSPricing_GaussianCopula(CDS1_USD_XR_MARGINAL_CreditCurve,
                                CDS2_USD_XR_MARGINAL_CreditCurve,
                                CDS3_USD_XR_MARGINAL_CreditCurve,
                                CDS4_USD_XR_MARGINAL_CreditCurve,
                                CDS5_USD_XR_MARGINAL_CreditCurve,
                                YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1,50000)


UniformCorrelationMatrix = function(rho,n) matrix(rho,nrow=n,ncol=n) + (1-rho)*diag(n)


objective_function = function(x) {
  res = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 2, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 3, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 4, marketprice = x),
                               new ("CreditDefaultSwap", maturity = 5, marketprice = x)),0.40,yieldcurve_test)
  return(res@hazardrate[5]-0.01)
}
uniroot(objective_function,lower=0,upper=10000)


yieldcurve_test = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.05*x)))

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

FTDS_GaussianCopula(c(cc,cc),2,yieldcurve_test,UniformCorrelationMatrix(0.3,2),0.40,100)
FTDS_GaussianCopula(c(cc,cc,cc,cc,cc),yieldcurve_test,UniformCorrelationMatrix(0.3,5),0.40,10000)


spread_50 = 50
cc_50 = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 2, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 3, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 4, marketprice = spread_50),
                               new ("CreditDefaultSwap", maturity = 5, marketprice = spread_50)),0.40,yieldcurve_test)

FTDS_GaussianCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_test,UniformCorrelationMatrix(0.6,5),0.40,10000)


spread_350 = 350 
cc_350 = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 2, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 3, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 4, marketprice = spread_350),
                                new ("CreditDefaultSwap", maturity = 5, marketprice = spread_350)),0.40,yieldcurve_test)

FTDS_GaussianCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50,cc_350,cc_350,cc_350,cc_350,cc_350),yieldcurve_test,UniformCorrelationMatrix(0.0,10),0.40,10000)


