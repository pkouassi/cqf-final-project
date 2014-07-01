#Sampling from student t copula

spread_studentt = BasketCDSPricing_StudentTCopula(CDS1_USD_XR_MARGINAL_CreditCurve,
                                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                                  CDS5_USD_XR_MARGINAL_CreditCurve,
                                                  YieldCurve,CorrelationMatrix_GaussianCopula,degree_freedom,0.40,1,50000)


