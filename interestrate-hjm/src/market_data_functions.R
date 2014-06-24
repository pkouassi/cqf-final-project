parseHistoricalForwardCurve = function(shortend_filename,longend_filename) {
  forward_curve_shortend=read.csv(paste(getwd(),shortend_filename,sep=""), skip=3,header = TRUE,stringsAsFactors = FALSE)
  forward_curve_longend=read.csv(paste(getwd(),longend_filename,sep=""), skip=3,header = TRUE,stringsAsFactors = FALSE)  
  date_format = "%d %b %y"
  
  #count number of observations
  k=0
  for (i in seq(1,nrow(forward_curve_longend))) {
    date = as.Date(forward_curve_longend[i,1],date_format)
    if (!is.na(date)) {
      if (forward_curve_longend[i,2] != "") {
        k = k+1    
      }
    }       
  }
  nbrecords = k
  
  #initialze matrix
  HistoricalForwardCurve = matrix(NA,nrow=nbrecords,ncol=51,byrow = TRUE);
  colnames(HistoricalForwardCurve) = c(0.08, seq(0.5,25,by=0.5))
  DateForwardCurve =  rep(as.Date("01 Jan 70",date_format),nbrecords)
  
  #populate matrix
  #we asumme that short-end file and long-end have same data structure and same dates
  k=1
  for (i in seq(1,nrow(forward_curve_longend))) {
    date = as.Date(forward_curve_longend[i,1],date_format)
    if (!is.na(date)) {
      if (forward_curve_shortend[i,2] != "") {    
        #make sure date are aligned between the shortend file and longend file
        if (forward_curve_shortend[i,1] == forward_curve_longend[i,1]) {
          DateForwardCurve[k] = date
          HistoricalForwardCurve[k,1] = as.numeric(forward_curve_shortend[i,2])
          for (j in seq(2,51)) HistoricalForwardCurve[k,j] = as.numeric(forward_curve_longend[i,j])        
        }
        else {
          cat("Dates are not aligned between shortend file and longend file at record [",i,"]\n")
        }      
        k = k+1   
      }
      else {
        cat("No forward curve data for", format(as.Date(forward_curve_longend[i,1],date_format),date_format),"\n")
      }
    }
  }
  
  cat("UK instantaneous commercial bank liability forward curve loaded between",format(as.Date(min(DateForwardCurve),origin = as.Date("01 Jan 70",date_format)),date_format),"and",format(as.Date(max(DateForwardCurve),origin = as.Date("01 Jan 70",date_format)),date_format),"\n")
  return(list(date=DateForwardCurve,forwardcurve=HistoricalForwardCurve))
}

parseForwardCurve = function(date,shortend_filename,longend_filename) {
  #date = as.Date("2014-05-30","%Y-%m-%d")
  #shortend_filename ="/../data/ukblc05_mdaily_fwdcurve_shortend.csv"
  
  forward_curve_shortend=read.csv(paste(getwd(),shortend_filename,sep=""), skip=3,header = TRUE,stringsAsFactors = FALSE)
  date_format = "%d %b %y"
  forward_curve_shortend[,1] = as.Date(forward_curve_shortend[,1],date_format)
  forward_curve_shortend = forward_curve_shortend[-1,] #suppress first line that does not contain data
  
  #initialize the vectors
  ForwardCurve = as.numeric(forward_curve_shortend[forward_curve_shortend[,1] == date,2:ncol(forward_curve_shortend)])
  ForwardCurveDate = seq(1/12,5,by=1/12)
  
  #ForwardCurve = matrix(NA,nrow=1,ncol=(ncol(forward_curve_shortend)-1),byrow = TRUE);
  #colnames(ForwardCurve) = 

  #populate matrix
  #ForwardCurve[1,] = as.numeric(forward_curve_shortend[forward_curve_shortend[,1] == date,2:ncol(forward_curve_shortend)])  
  
  return(list(time=ForwardCurveDate,rate=ForwardCurve))
  return()
}

parseOISSpotCurve = function(date,ois_spotcurve_filename) {
  #date = as.Date("2014-05-30","%Y-%m-%d")
  #ois_spotcurve_filename ="/../data/ukois09_mdaily_spotcurve.csv"
  
  ois_spot_curve=read.csv(paste(getwd(),ois_spotcurve_filename,sep=""), skip=3,header = TRUE,stringsAsFactors = FALSE)
  date_format = "%d %b %y"
  ois_spot_curve[,1] = as.Date(ois_spot_curve[,1],date_format)
  ois_spot_curve = ois_spot_curve[-1,] #suppress first line that does not contain data
  
  k=1
  for (i in seq(1,length(ois_spot_curve[,1]))) {
    if (!is.na(ois_spot_curve[i,1])) {
      k = k +1
    }
    else {
      break
    }
  }
  nbrecords = k-1
  
  ois_spot_curve = ois_spot_curve[1:nbrecords,] #filter blank records at the end of the file
  
  #initialize the vectors
  OISSpotCurve = as.numeric(ois_spot_curve[ois_spot_curve[,1] == date,2:ncol(ois_spot_curve)])  
  OISSpotCurveDate = seq(1/12,5,by=1/12)
  
  return(list(time=OISSpotCurveDate,rate=OISSpotCurve))
}

#retrieve discount factor for a specific date
GetDiscountFactor = function(YieldCurve,t) {
  min_time = min(YieldCurve@time)
  min_time_index = which.min(YieldCurve@time)
  max_time = max(YieldCurve@time)
  max_time_index = which.max(YieldCurve@time)
  
  result = NA
  
  if (length(t)==1) {
    if (t < 0) {
      cat("Warning: t is negative. discountfactor not calculated for this case")
    }
    else if (t == 0) {
      result = 1 #df of t=0 is 1
    }
    else if (t>0 && t<min_time) {
      #df of t=0 is 1
      result = 1 + (YieldCurve@discountfactor[min_time_index]-1)*(t/min_time)
    }
    else if (t >= max_time) {
      result = YieldCurve@discountfactor[max_time_index]
    }
    else {
      #i.e t falls between 2 maturity for which we have the discount factor
      for (i in seq(1,length(YieldCurve@time)-1)) {
        if (t>= YieldCurve@time[i] && t<YieldCurve@time[i+1]) {
          result = YieldCurve@discountfactor[i] + (YieldCurve@discountfactor[i+1]-YieldCurve@discountfactor[i])*((t-YieldCurve@time[i])/(YieldCurve@time[i+1]-YieldCurve@time[i]))
        }                                                                                                                 
      }
    }  
  }
  return (result)  
}

#Vectorized version of GetDiscountFactor
GetDiscountFactorVector = function(YieldCurve,t_array){
  return(sapply(t_array,GetDiscountFactor,YieldCurve=YieldCurve))
}

