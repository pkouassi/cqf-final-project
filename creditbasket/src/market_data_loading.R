#Loading Historical Yield Curve
HistoricalYieldCurveMatrix = parseHistoricalYieldCurve("/../data/HistoricYieldCurveWeekly1Y-2013-2014.csv")

#Loading Historical Yield Curve
BMY_USD_XR=parseHistoricalCreditData("C://temp//markit","BMY","USD","XR")
DELL_USD_XR=parseHistoricalCreditData("C://temp//markit","DELLN","USD","XR")
HP_USD_XR=parseHistoricalCreditData("C://temp//markit","HPQ","USD","XR")
IBM_USD_XR=parseHistoricalCreditData("C://temp//markit","IBM","USD","XR")
PFE_USD_XR=parseHistoricalCreditData("C://temp//markit","PFE","USD","XR")

#only keep data from Monday 6-May-2013 to Friday 23-May-2014
BMY_USD_XR = BMY_USD_XR[BMY_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & BMY_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
DELL_USD_XR = DELL_USD_XR[DELL_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & DELL_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
HP_USD_XR = HP_USD_XR[HP_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & HP_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
IBM_USD_XR = IBM_USD_XR[IBM_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & IBM_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
PFE_USD_XR = PFE_USD_XR[PFE_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & PFE_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]

cbind(BMY_USD_XR$Date,DELL_USD_XR$Date,HP_USD_XR$Date,IBM_USD_XR$Date,PFE_USD_XR$Date)

#check that dates are all aligned
(BMY_USD_XR$Date == DELL_USD_XR$Date)
(BMY_USD_XR$Date == HP_USD_XR$Date)
(BMY_USD_XR$Date == IBM_USD_XR$Date)
(BMY_USD_XR$Date == PFE_USD_XR$Date)
