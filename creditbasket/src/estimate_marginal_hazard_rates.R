#Estimation of hazard rates (empirical marginal distribution)
CDS1_USD_XR_MARGINAL = CDS1_USD_XR[CDS1_USD_XR$Date==as.Date("30-APR-2014","%d-%b-%Y") ,]
CDS2_USD_XR_MARGINAL = CDS2_USD_XR[CDS2_USD_XR$Date==as.Date("30-APR-2014","%d-%b-%Y") ,]
CDS3_USD_XR_MARGINAL = CDS3_USD_XR[CDS3_USD_XR$Date==as.Date("30-APR-2014","%d-%b-%Y") ,]
CDS4_USD_XR_MARGINAL = CDS4_USD_XR[CDS4_USD_XR$Date==as.Date("30-APR-2014","%d-%b-%Y") ,]
CDS5_USD_XR_MARGINAL = CDS5_USD_XR[CDS5_USD_XR$Date==as.Date("30-APR-2014","%d-%b-%Y") ,]

#Get Yield Curve
YieldCurve = getYieldCurve(HistoricalYieldCurveMatrix,as.Date("30-APR-2014","%d-%b-%Y"))

#Credit Curve Bootstrapping
CDS1_USD_XR_MARGINAL_CreditCurve = BootstrapHistoricCreditCurve(CDS1_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
CDS2_USD_XR_MARGINAL_CreditCurve = BootstrapHistoricCreditCurve(CDS2_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
CDS3_USD_XR_MARGINAL_CreditCurve = BootstrapHistoricCreditCurve(CDS3_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
CDS4_USD_XR_MARGINAL_CreditCurve = BootstrapHistoricCreditCurve(CDS4_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]
CDS5_USD_XR_MARGINAL_CreditCurve = BootstrapHistoricCreditCurve(CDS5_USD_XR_MARGINAL,HistoricalYieldCurveMatrix)[[1,"CreditCurve"]]

HazardRatesMatrix = cbind(CDS1_USD_XR_MARGINAL_CreditCurve@hazardrate,CDS2_USD_XR_MARGINAL_CreditCurve@hazardrate,CDS3_USD_XR_MARGINAL_CreditCurve@hazardrate,CDS4_USD_XR_MARGINAL_CreditCurve@hazardrate,CDS5_USD_XR_MARGINAL_CreditCurve@hazardrate)
colnames(HazardRatesMatrix) = AssetTicker

