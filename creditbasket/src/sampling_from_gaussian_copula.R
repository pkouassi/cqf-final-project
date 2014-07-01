#Sampling from gaussian copula
spread_gaussian = BasketCDSPricing_GaussianCopula(CDS1_USD_XR_MARGINAL_CreditCurve,
                                CDS2_USD_XR_MARGINAL_CreditCurve,
                                CDS3_USD_XR_MARGINAL_CreditCurve,
                                CDS4_USD_XR_MARGINAL_CreditCurve,
                                CDS5_USD_XR_MARGINAL_CreditCurve,
                                YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1,50000)


UniformCorrelationMatrix = function(rho,n) matrix(rho,nrow=n,ncol=n) + (1-rho)*diag(n)
corrmat = UniformCorrelationMatrix(sqrt(0.3),5)
cc = BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = 400.00),
                       new ("CreditDefaultSwap", maturity = 2, marketprice = 420.00),
                       new ("CreditDefaultSwap", maturity = 3, marketprice = 440.00),
                       new ("CreditDefaultSwap", maturity = 4, marketprice = 460.00),
                       new ("CreditDefaultSwap", maturity = 5, marketprice = 480.00)),0.40,YieldCurve)

BasketCDSPricing_GaussianCopula(cc,cc,cc,cc,cc,corrmat,0.40,1,50000)