#==============================================================================
# title           :market_data_loading.R
# description     :This script load his cds spread and hist yield curve
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# Loading Historical Yield Curve
HistoricalYieldCurveMatrix = parseHistoricalYieldCurve("/../data/HistoricYieldCurveWeekly2011-2014.csv")

# Loading Historical CDS spreads from markit files 
# Markit files are not provided; only export with selected reference names are provided
# AssetTicker = c("BMY","TMSNRC","HPQ","IBM","PFE")
# start_date = as.Date("30-APR-2012","%d-%b-%Y")
# end_date = as.Date("30-APR-2014","%d-%b-%Y")
# CDS1_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[1],"USD","XR",start_date,end_date)
# CDS2_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[2],"USD","XR",start_date,end_date)
# CDS3_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[3],"USD","XR",start_date,end_date)
# CDS4_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[4],"USD","XR",start_date,end_date)
# CDS5_USD_XR=parseHistoricalCreditData("C://temp//markit",AssetTicker[5],"USD","XR",start_date,end_date)

# Load credit data (weekly historical CDS spreads-29APR2011 to 30APR2014)
CDS1_USD_XR = import_dataframe("BMY_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS2_USD_XR = import_dataframe("TMSNRC_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS3_USD_XR = import_dataframe("HPQ_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS4_USD_XR = import_dataframe("IBM_USD_XR_weekly_29APR2011_30APR2014.csv")
CDS5_USD_XR = import_dataframe("PFE_USD_XR_weekly_29APR2011_30APR2014.csv")

# Load credit data (daily historical CDS spreads-30APR2012 to 30APR2014)
# CDS1_USD_XR = import_dataframe("BMY_USD_XR_30APR2012_30APR2014.csv")
# CDS2_USD_XR = import_dataframe("TMSNRC_USD_XR_30APR2012_30APR2014.csv")
# CDS3_USD_XR = import_dataframe("HPQ_USD_XR_30APR2012_30APR2014.csv")
# CDS4_USD_XR = import_dataframe("IBM_USD_XR_30APR2012_30APR2014.csv")
# CDS5_USD_XR = import_dataframe("PFE_USD_XR_30APR2012_30APR2014.csv")

# Load credit data (daily historical CDS spreads-29APR2013 to 30APR2014)
# CDS1_USD_XR = import_dataframe("BMY_USD_XR_29APR2013_30APR2014.csv")
# CDS2_USD_XR = import_dataframe("TMSNRC_USD_XR_29APR2013_30APR2014.csv")
# CDS3_USD_XR = import_dataframe("HPQ_USD_XR_29APR2013_30APR2014.csv")
# CDS4_USD_XR = import_dataframe("IBM_USD_XR_29APR2013_30APR2014.csv")
# CDS5_USD_XR = import_dataframe("PFE_USD_XR_29APR2013_30APR2014.csv")

