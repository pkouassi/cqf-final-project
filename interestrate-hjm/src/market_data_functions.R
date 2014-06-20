
current_dir = "P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/src"
setwd(current_dir)


shortend_filename = "/../data/ukblc05_mdaily_fwdcurve_shortend.csv"
longend_filename = "/../data/ukblc05_mdaily_fwdcurve_longend.csv"
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
HistoricalForwardCurve = matrix(NA,nrow=nbrecords,ncol=52,byrow = TRUE);
colnames(HistoricalForwardCurve) = c("Date", 0.08, seq(0.5,25,by=0.5))

#populate matrix
#we asumme that short-end file and long-end have same data structure and same dates
k=1
for (i in seq(1,nrow(forward_curve_longend))) {
  date = as.Date(forward_curve_longend[i,1],date_format)
  if (!is.na(date)) {
    if (forward_curve_shortend[i,2] != "") {    
      #make sure date are aligned between the shortend file and longend file
      if (forward_curve_shortend[i,1] == forward_curve_longend[i,1]) {
        HistoricalForwardCurve[k,1] = date
        HistoricalForwardCurve[k,2] = forward_curve_shortend[i,2]
        for (j in seq(3,52)) HistoricalForwardCurve[k,j] = forward_curve_longend[i,j-1]        
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









k=0
for (i in seq(1,nrow(forward_curve_longend))) {
  date = as.Date(forward_curve_longend[i,1],date_format)
  if (!is.na(date)) {
    if (forward_curve_longend[i,2] != "") {
      k = k+1    
    }
    else {
      cat("No forward curve data for", format(as.Date(forward_curve_longend[i,1],date_format),date_format),"\n")
    }
  }       
}

HistoricalForwardCurve = matrix(list(),nrow=length(unique(historic_yielcurve$Date)),ncol=2,byrow = TRUE);
colnames(HistoricYieldCurveMatrix) = c("Date", "YieldCurve")




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
  HistoricalForwardCurve = matrix(NA,nrow=nbrecords,ncol=52,byrow = TRUE);
  colnames(HistoricalForwardCurve) = c("Date", 0.08, seq(0.5,25,by=0.5))
  
  #populate matrix
  #we asumme that short-end file and long-end have same data structure and same dates
  k=1
  for (i in seq(1,nrow(forward_curve_longend))) {
    date = as.Date(forward_curve_longend[i,1],date_format)
    if (!is.na(date)) {
      if (forward_curve_shortend[i,2] != "") {    
        #make sure date are aligned between the shortend file and longend file
        if (forward_curve_shortend[i,1] == forward_curve_longend[i,1]) {
          HistoricalForwardCurve[k,1] = date
          HistoricalForwardCurve[k,2] = as.numeric(forward_curve_shortend[i,2])
          for (j in seq(3,52)) HistoricalForwardCurve[k,j] = as.numeric(forward_curve_longend[i,j-1])        
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
  
  cat("UK instantaneous commercial bank liability forward curve loaded between",format(as.Date(min(tt[,1]),origin = as.Date("01 Jan 70",date_format)),date_format),"and",format(as.Date(max(tt[,1]),origin = as.Date("01 Jan 70",date_format)),date_format),"\n")
  return(HistoricalForwardCurve)
}