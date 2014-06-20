#YieldCurves

HistoricYieldCurveMatrix = parseHistoricYieldCurve("/../data/HistoricYieldCurveWeekly1Y-2013-2014.csv")

plot_x_lim = c(1,5);
plot_y_lim = c(0.9,1);
for (i in seq(1,nrow(HistoricYieldCurveMatrix))) {
 data = HistoricYieldCurveMatrix[[i,2]]
 par(new=TRUE);
 plot(data@time,data@discountfactor,type="l",xlim=plot_x_lim, ylim=plot_y_lim)
}

getYieldCurve(HistoricYieldCurveMatrix,as.Date("12-OCT-2013","%d-%b-%Y"))