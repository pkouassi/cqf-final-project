#Loading Historical Yield Curve
HistoricalYieldCurveMatrix = parseHistoricalYieldCurve("/../data/HistoricYieldCurveWeekly1Y-2013-2014.csv")

#Loading Historical Yield Curve
AssetTicker = c("BMY","RSH","HPQ","IBM","PFE")
start_date = as.Date("06-MAY-2013","%d-%b-%Y")
end_date = as.Date("23-MAY-2014","%d-%b-%Y")

#CDS1_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[1],"USD","XR",start_date,end_date)
#CDS2_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[2],"USD","XR",start_date,end_date)
#CDS3_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[3],"USD","XR",start_date,end_date)
#CDS4_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[4],"USD","XR",start_date,end_date)
#CDS5_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[5],"USD","XR",start_date,end_date)

#export_dataframe(CDS1_USD_XR,"BMY_USD_XR_06MAY2013_23MAY2014.csv")
#export_dataframe(CDS2_USD_XR,"RSH_USD_XR_06MAY2013_23MAY2014.csv")
#export_dataframe(CDS2_USD_XR,"DELLN_USD_XR_06MAY2013_23MAY2014.csv")
#export_dataframe(CDS3_USD_XR,"HPQ_USD_XR_06MAY2013_23MAY2014.csv")
#export_dataframe(CDS4_USD_XR,"IBM_USD_XR_06MAY2013_23MAY2014.csv")
#export_dataframe(CDS5_USD_XR,"PFE_USD_XR_06MAY2013_23MAY2014.csv")

CDS1_USD_XR = import_dataframe("BMY_USD_XR_06MAY2013_23MAY2014.csv")
CDS2_USD_XR = import_dataframe("RSH_USD_XR_06MAY2013_23MAY2014.csv")
#CDS2_USD_XR = import_dataframe("DELLN_USD_XR_06MAY2013_23MAY2014.csv")
CDS3_USD_XR = import_dataframe("HPQ_USD_XR_06MAY2013_23MAY2014.csv")
CDS4_USD_XR = import_dataframe("IBM_USD_XR_06MAY2013_23MAY2014.csv")
CDS5_USD_XR = import_dataframe("PFE_USD_XR_06MAY2013_23MAY2014.csv")


#only keep data from Monday 6-May-2013 to Friday 23-May-2014
#CDS1_USD_XR = CDS1_USD_XR[CDS1_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & CDS1_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
#CDS2_USD_XR = CDS2_USD_XR[CDS2_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & CDS2_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
#CDS3_USD_XR = CDS3_USD_XR[CDS3_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & CDS3_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
#CDS4_USD_XR = CDS4_USD_XR[CDS4_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & CDS4_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]
#CDS5_USD_XR = CDS5_USD_XR[CDS5_USD_XR$Date>=as.Date("06-MAY-2013","%d-%b-%Y") & CDS5_USD_XR$Date<=as.Date("23-MAY-2014","%d-%b-%Y"),]

cbind(CDS1_USD_XR$Date,CDS2_USD_XR$Date,CDS3_USD_XR$Date,CDS4_USD_XR$Date,CDS5_USD_XR$Date)

#check that dates are all aligned
(CDS1_USD_XR$Date == CDS2_USD_XR$Date)
(CDS1_USD_XR$Date == CDS3_USD_XR$Date)
(CDS1_USD_XR$Date == CDS4_USD_XR$Date)
(CDS1_USD_XR$Date == CDS5_USD_XR$Date)
