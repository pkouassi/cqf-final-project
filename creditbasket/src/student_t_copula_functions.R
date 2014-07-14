#==============================================================================
# title           :student_t_copula_functions.R
# description     :Defines the pricing function for Student t Copula sampling
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

BasketCDSPricing_StudentTCopula = function(CreditCurveCollection,DiscountCurve,CorrelationMatrix,DegreeFreedom,RecoveryRate,NumberSimulation=300000,GenType="rnorm") {
  Maturity = 5
  NumberCDS = length(CreditCurveCollection)
  
  CholStatus = try(A_studentt <- t(chol(CorrelationMatrix)),silent=FALSE)
  CholError = ifelse(class(CholStatus) == "try-error", TRUE, FALSE)
  if (CholError) {
    warning("CorrelationMatrix is not positive definite. BasketCDSPricing stops here...")
    return()
  }
  
  # Pseudo random genetor or quasi random generator
  if (GenType == "rnorm") {
    ZMatrix_studentt = matrix(rnorm(NumberSimulation*NumberCDS, mean = 0, sd = 1),ncol=NumberCDS,nrow=NumberSimulation,byrow=FALSE)
  }
  else if (GenType == "sobol") {
    ZMatrix_studentt = sobol(NumberSimulation, dim = NumberCDS, normal = TRUE, scrambling = 3)
  }
  else if (GenType == "halton") {
    ZMatrix_studentt = halton(NumberSimulation, dim = NumberCDS, normal = TRUE)
  }
  else if (GenType == "nag-sobol") {
    ZMatrix_studentt = quasirandom.nag(NumberSimulation,NumberCDS,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else if (GenType == "nag-niederreiter") {
    ZMatrix_studentt = quasirandom.nag(NumberSimulation,NumberCDS,"niederreiter","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else if (GenType == "nag-faure") {
    ZMatrix_studentt = quasirandom.nag(NumberSimulation,NumberCDS,"faure","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  }
  else {
    ZMatrix_studentt = matrix(rnorm(NumberSimulation*NumberCDS, mean = 0, sd = 1),ncol=NumberCDS,nrow=NumberSimulation,byrow=FALSE)
  }
  
  #ZMatrix_studentt = matrix(rnorm(NumberSimulation*NumberCDS, mean = 0, sd = 1),ncol=NumberCDS,nrow=NumberSimulation,byrow=FALSE)
  #using sobol numbers
  ZMatrix_studentt = rnorm.sobol(n = NumberSimulation, dimension = NumberCDS , scrambling = 3)
  #ZMatrix_studentt = quasirandom.nag(NumberSimulation,NumberCDS,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
  
  YMatrix_studentt = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  XMatrix_studentt = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  ChiSquare = rchisq(NumberSimulation, DegreeFreedom)
  #adjustment by the chi-squared number and apply correlation
  for (i in seq(1,NumberSimulation)) {
    YMatrix_studentt[i,] = ZMatrix_studentt[i,]/(sqrt(ChiSquare[i]/DegreeFreedom))
    XMatrix_studentt[i,] = t(A_studentt %*% YMatrix_studentt[i,])# t() in order to keep X as a row vector
  }
  
  #convert to uniform correlated vector by applying the student t cdf
  UMatrix_studentt = pt(XMatrix_studentt, df = DegreeFreedom)
  
  TauMatrix_studentt = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  for (i in seq(1,NumberCDS)) {
    TauMatrix_studentt[,i] = ConvertToDefaultTime(CreditCurveCollection[[i]],UMatrix_studentt[,i])
  }
  
  #Matrix that will keep track of the single-name Premium Leg / Default Leg
  SingleName_PremiumLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  SingleName_DefaultLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  SingleName_FairSpread = rep(NA,NumberCDS)
  #Matrix that will keep track of the k-th basket CDS Premium Leg / Default Leg
  Basket_PremiumLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  Basket_DefaultLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
  Basket_FairSpread = rep(NA,NumberCDS)
  
  #Monte Carlo Loop
  for (i in seq(1,NumberSimulation)) {
    if ((i/NumberSimulation*100)%%25 == 0) cat((i/NumberSimulation)*100,"% ...\n")
    nbdefault = 0
    
    # Calculate Single-Name CDS Premium Leg / Default Leg (to ensure everything is in order)
    for (j in seq(1,NumberCDS)) {
      if (TauMatrix_studentt[i,j] == Inf) {
        # no default on jth CDS
        # premium leg
        premium_leg = 0
        for (l in seq(1,Maturity)) {
          premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,l)
        }                
        if (i==1) SingleName_PremiumLeg[i,j] = premium_leg
        else SingleName_PremiumLeg[i,j] = ((i-1)*SingleName_PremiumLeg[i-1,j] + premium_leg)/i
        
        #default leg
        default_leg = 0
        if (i==1) SingleName_DefaultLeg[i,j] = default_leg
        else SingleName_DefaultLeg[i,j] = ((i-1)*SingleName_DefaultLeg[i-1,j] + default_leg)/i      
      }
      else {
        # default on jth CDS
        nbdefault = nbdefault + 1
        # premium leg
        premium_leg = 0   
        l=1
        while (l<TauMatrix_studentt[i,j] & l<Maturity) {
          premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,j) * 1
          l = l+1
        }
        premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,TauMatrix_studentt[i,j]) * (TauMatrix_studentt[i,j]-(l-1))
        if (i==1) SingleName_PremiumLeg[i,j] = premium_leg
        else SingleName_PremiumLeg[i,j] = ((i-1)*SingleName_PremiumLeg[i-1,j] + premium_leg)/i
        
        # default leg
        default_leg = (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,TauMatrix_studentt[i,j])
        if (i==1) SingleName_DefaultLeg[i,j] = default_leg
        else SingleName_DefaultLeg[i,j] = ((i-1)*SingleName_DefaultLeg[i-1,j] + default_leg)/i  
      }
    }
    
    # Calculate Basket CDS Premium Leg / Default Leg 
    # vector of tau sorted (increasing)
    sorted_tau = sort(TauMatrix_studentt[i,])
    for (k in seq(1,NumberCDS)) {
      if (nbdefault > 0 && sorted_tau[k] != Inf) {
        # if there is at least 1 default (>0) and there is a kth default
        # premium leg
        premium_leg = ComputePremiumLeg(DiscountCurve,NumberCDS,k,sorted_tau)
        if (i==1) Basket_PremiumLeg[i,k]  = premium_leg
        else Basket_PremiumLeg[i,k] = ((i-1)*Basket_PremiumLeg[i-1,k] + premium_leg)/i
        # default leg
        default_leg = (1-RecoveryRate) * GetDiscountFactor(DiscountCurve,sorted_tau[k]) * (1/NumberCDS)      
        if (i==1) Basket_DefaultLeg[i,k]  = default_leg
        else Basket_DefaultLeg[i,k] = ((i-1)*Basket_DefaultLeg[i-1,k] + default_leg)/i      
      }
      else {
        # no default
        # premium leg
        premium_leg = 0
        for (l in seq(1,Maturity)) {
          premium_leg = premium_leg + GetDiscountFactor(DiscountCurve,l)
        }        
        if (i==1) Basket_PremiumLeg[i,k]  = premium_leg
        else Basket_PremiumLeg[i,k] = ((i-1)*Basket_PremiumLeg[i-1,k] + premium_leg)/i
        # default leg
        default_leg = 0
        if (i==1) Basket_DefaultLeg[i,k] = default_leg
        else Basket_DefaultLeg[i,k] = ((i-1)*Basket_DefaultLeg[i-1,k] + default_leg)/i       
      }
    }
  }
  
  for (j in seq(1,NumberCDS)) {
    cat("CDS",j,"===>","default_leg=",SingleName_DefaultLeg[NumberSimulation,j],"premium_leg=",SingleName_PremiumLeg[NumberSimulation,j],"par_spread=",SingleName_FairSpread[j] <- SingleName_DefaultLeg[NumberSimulation,j]/SingleName_PremiumLeg[NumberSimulation,j]*10000,"\n")
  }
  
  for (k in seq(1,NumberCDS)) {
    cat(k,"th to default basket ===>","default_leg=",Basket_DefaultLeg[NumberSimulation,k],"premium_leg=",Basket_PremiumLeg[NumberSimulation,k],"par_spread=",Basket_FairSpread[k] <- Basket_DefaultLeg[NumberSimulation,k]/Basket_PremiumLeg[NumberSimulation,k]*10000,"\n")
  }
  
  return(list(basket_spreads=Basket_FairSpread,singlename_spreads=SingleName_FairSpread,basket_sim=cbind(Basket_DefaultLeg,Basket_PremiumLeg),singlename_sim=cbind(SingleName_DefaultLeg,SingleName_PremiumLeg),tau=TauMatrix_studentt))
  
}

