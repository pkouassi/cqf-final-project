#Loading Historical Yield Curve
HistoricalYieldCurveMatrix = parseHistoricalYieldCurve("/../data/HistoricYieldCurveWeekly2011-2014.csv")

#Loading Historical Yield Curve
AssetTicker = c("BMY","TMSNRC","HPQ","IBM","PFE")
start_date = as.Date("30-APR-2012","%d-%b-%Y")
end_date = as.Date("30-APR-2014","%d-%b-%Y")

#CDS1_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[1],"USD","XR",start_date,end_date)
#CDS2_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[2],"USD","XR",start_date,end_date)
#CDS3_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[3],"USD","XR",start_date,end_date)
#CDS4_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[4],"USD","XR",start_date,end_date)
#CDS5_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[5],"USD","XR",start_date,end_date)

#CDS1_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[1],"USD","XR",start_date,end_date,"weekly")
#CDS2_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[2],"USD","XR",start_date,end_date,"weekly")
#CDS3_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[3],"USD","XR",start_date,end_date,"weekly")
#CDS4_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[4],"USD","XR",start_date,end_date,"weekly")
#CDS5_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[5],"USD","XR",start_date,end_date,"weekly")

#export_dataframe(CDS1_USD_XR,"BMY_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS2_USD_XR,"RSH_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS2_USD_XR,"RSH_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS2_USD_XR,"GOOGLE_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS3_USD_XR,"HPQ_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS4_USD_XR,"IBM_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS5_USD_XR,"PFE_USD_XR_29APR2013_30APR2014.csv")

#export_dataframe(CDS1_USD_XR,"KO_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS2_USD_XR,"LEVI_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS3_USD_XR,"NSINO_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS4_USD_XR,"QUCAN_USD_XR_29APR2013_30APR2014.csv")
#export_dataframe(CDS5_USD_XR,"TMSNRC_USD_XR_29APR2013_30APR2014.csv")

#export_dataframe(CDS1_USD_XR,"BMY_USD_XR_30APR2012_30APR2014.csv")
#export_dataframe(CDS2_USD_XR,"TMSNRC_USD_XR_30APR2012_30APR2014.csv")
#export_dataframe(CDS3_USD_XR,"HPQ_USD_XR_30APR2012_30APR2014.csv")
#export_dataframe(CDS4_USD_XR,"IBM_USD_XR_30APR2012_30APR2014.csv")
#export_dataframe(CDS5_USD_XR,"PFE_USD_XR_30APR2012_30APR2014.csv")

# 
# CDS1_USD_XR = import_dataframe("BMY_USD_XR_29APR2013_30APR2014.csv")
# CDS2_USD_XR = import_dataframe("TMSNRC_USD_XR_29APR2013_30APR2014.csv")
# CDS3_USD_XR = import_dataframe("HPQ_USD_XR_29APR2013_30APR2014.csv")
# CDS4_USD_XR = import_dataframe("IBM_USD_XR_29APR2013_30APR2014.csv")
# CDS5_USD_XR = import_dataframe("PFE_USD_XR_29APR2013_30APR2014.csv")

#weekly data
CDS1_USD_XR = import_dataframe("BMY_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS2_USD_XR = import_dataframe("TMSNRC_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS3_USD_XR = import_dataframe("HPQ_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS4_USD_XR = import_dataframe("IBM_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS5_USD_XR = import_dataframe("PFE_USD_XR_weekly_29APR2011_30APR2014.csv")

#daily 2Y data
# CDS1_USD_XR = import_dataframe("BMY_USD_XR_30APR2012_30APR2014.csv")
# CDS2_USD_XR = import_dataframe("TMSNRC_USD_XR_30APR2012_30APR2014.csv")
# CDS3_USD_XR = import_dataframe("HPQ_USD_XR_30APR2012_30APR2014.csv")
# CDS4_USD_XR = import_dataframe("IBM_USD_XR_30APR2012_30APR2014.csv")
# CDS5_USD_XR = import_dataframe("PFE_USD_XR_30APR2012_30APR2014.csv")


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
