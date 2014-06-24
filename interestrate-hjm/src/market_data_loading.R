#Loading Historical Forward Curve
shortend_filename = "/../data/ukblc05_mdaily_fwdcurve_shortend.csv"
longend_filename = "/../data/ukblc05_mdaily_fwdcurve_longend.csv"
res = parseHistoricalForwardCurve(shortend_filename,longend_filename)
HistoricalForwardCurveDate = res$date
HistoricalForwardCurve = res$forwardcurve  

#Data filtration (too long period can lead to difficulty in PCA)
#we keep take from 1st May 2009 to 30th May 2014
start_date = as.Date("2011-06-01","%Y-%m-%d")
end_date = as.Date("2014-05-30","%Y-%m-%d")

HistoricalForwardCurve = HistoricalForwardCurve[(HistoricalForwardCurveDate>= start_date  & HistoricalForwardCurveDate<= end_date),]
HistoricalForwardCurveDate = HistoricalForwardCurveDate[HistoricalForwardCurveDate>= start_date & HistoricalForwardCurveDate<= end_date]
cat("we retain data between",format(start_date,"%d %b %Y"),"and",format(end_date,"%d %b %Y"),"\n")

#Load forward curve for valuation date (data more granular than for PCA; forward rate every month)
valuation_date = as.Date("2014-05-30","%Y-%m-%d")
ValuationDateForwardCurve = parseForwardCurve(valuation_date,shortend_filename,longend_filename)
plot(ValuationDateForwardCurve$time,ValuationDateForwardCurve$rate,type="l")

#Load OIS spot curve for valuation date
ois_spotcurve_filename ="/../data/ukois09_mdaily_spotcurve.csv"
ValuationDateOISSpotCurve = parseOISSpotCurve(valuation_date,ois_spotcurve_filename)
plot(ValuationDateOISSpotCurve$time,ValuationDateOISSpotCurve$rate,type="l")

#calculate discount factors and define yieldcurve
ValuationDateOISDiscountCurve = list(time=ValuationDateOISSpotCurve$time,discountfactor=exp(-1*ValuationDateOISSpotCurve$rate/100*ValuationDateOISSpotCurve$time))
ValuationDateOISYieldCurve = new ("YieldCurve", time = ValuationDateOISDiscountCurve$time, discountfactor = ValuationDateOISDiscountCurve$discountfactor)  
plot(ValuationDateOISDiscountCurve$time,ValuationDateOISDiscountCurve$discountfactor,type="l")
