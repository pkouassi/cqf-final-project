#Loading Historical Forward Curve
shortend_filename = "/../data/ukblc05_mdaily_fwdcurve_shortend.csv"
longend_filename = "/../data/ukblc05_mdaily_fwdcurve_longend.csv"
res = parseHistoricalForwardCurve(shortend_filename,longend_filename)
HistoricalForwardCurveDate = res$date
HistoricalForwardCurve = res$forwardcurve  