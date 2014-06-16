setClass("YieldCurve",
         representation(
           time = "vector",
           discountfactor = "vector")
)

#tmp=read.csv("P://CQF//FinalProject//FairSpreadBasketCDS//HistoricYieldCurve.csv", header = TRUE,stringsAsFactors = FALSE)
tmp=read.csv("P://CQF//FinalProject//git-root//finalproject//creditbasket//data//HistoricYieldCurveWeekly1Y-2013-2014.csv", header = TRUE,stringsAsFactors = FALSE)

HistYieldCurveMatrix = matrix(list(), 
                              nrow=length(unique(tmp$Date)),
                              ncol=2,
                              byrow = TRUE);

k=1
for (date in unique(tmp$Date)) {
  tmp2 = tmp[(tmp$Date == date),]
  cat("date number ",k,"\n")
  
  HistYieldCurveMatrix[[k,1]] = as.Date(date,"%Y-%m-%d") #format: 2014-04-23
  HistYieldCurveMatrix[[k,2]] = new ("YieldCurve", time = rep(NA,nrow(tmp2)), discountfactor = rep(NA,nrow(tmp2)))  
  
  for (i in seq(1,nrow(tmp2))) {
    cat("-- record number ",i,"\n")
    cat("-- Id= ",tmp2$Id[i],"\n")
    cat("-- DF= ",tmp2$DiscountFactor[i],"\n")
    
    if (tmp2$Id[i] == "S0023D 1D BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 1/365
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
      
    }
    else if (tmp2$Id[i] == "S0023D 2D BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 2/365
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 1W BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 7/365
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 1M BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 1/12
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 2M BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 2/12
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 3M BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 3/12
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 6M BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 6/12
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 1Y BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 1
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 2Y BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 2
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 3Y BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 3
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 4Y BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 4
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 5Y BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 5
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }
    else if (tmp2$Id[i] == "S0023D 6Y BLC2") {
      HistYieldCurveMatrix[[k,2]]@time[i] = 6
      HistYieldCurveMatrix[[k,2]]@discountfactor[i] = as.numeric(tmp2$DiscountFactor[i])
    }  
  }  

  k = k+1
}

#sort by date
HistYieldCurveMatrix = HistYieldCurveMatrix[order(as.numeric(HistYieldCurveMatrix[,1])),]

plot_x_lim = c(1,5);
plot_y_lim = c(0.9,1);
for (i in seq(1,nrow(HistYieldCurveMatrix))) {
  data = HistYieldCurveMatrix[[i,2]]
  par(new=TRUE);
  plot(data@time,data@discountfactor,type="l",xlim=plot_x_lim, ylim=plot_y_lim)
}

getYieldCurve = function(histyieldcurve,date) {
  result = NA
  #cat("requested date=",date,"\n")
  for (i in seq(1,nrow(histyieldcurve))) {
    if (i == nrow(histyieldcurve)) {
      if (date == histyieldcurve[[i,1]]) {
        result = histyieldcurve[[i,2]]
        break
      }
    }
    else {
      if (date>=histyieldcurve[[i,1]] && date<histyieldcurve[[i+1,1]]) 
      {
        result = histyieldcurve[[i,2]]
        break
      }
    }
  }
  return(result)     
}


#getYieldCurve(HistYieldCurveMatrix,as.Date("12-OCT-2013","%d-%b-%Y"))


