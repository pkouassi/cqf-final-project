yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.00*x)))
GetFlatCreditCurve = function(x,yc) {
  return(BootstrapCreditCurve(c(new ("CreditDefaultSwap", maturity = 1, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 2, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 3, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 4, marketprice = x),
                                new ("CreditDefaultSwap", maturity = 5, marketprice = x)),0.40,yc))
}
cc_60 = GetFlatCreditCurve(60,yieldcurve_flat)
cc_80 = GetFlatCreditCurve(60,yieldcurve_flat)
cc_IBM = GetFlatCreditCurve(40.06,yieldcurve_bbg)
cc_PFI = GetFlatCreditCurve(23.44,yieldcurve_bbg)
cc_DEL = GetFlatCreditCurve(244.88,yieldcurve_bbg)
cc_HPQ = GetFlatCreditCurve(22.00,yieldcurve_bbg)
cc_BMY = GetFlatCreditCurve(70.38,yieldcurve_bbg)

CreditCurveCollection = c(cc_60,cc_60,cc_60,cc_60,cc_60)
#CreditCurveCollection = c(cc_IBM,cc_PFI,cc_DEL,cc_HPQ,cc_BMY)

Maturity = 5
RecoveryRate = 0.4
UniformCorrelationMatrix = function(rho,n) matrix(rho,nrow=n,ncol=n) + (1-rho)*diag(n)
NumberSimulation = 50000
NumberCDS = length(CreditCurveCollection)
CorrelationMatrix = UniformCorrelationMatrix(0.3,5)

##############
A_gaussian = t(chol(CorrelationMatrix))

ZMatrix_gaussian = matrix(rnorm(NumberSimulation*NumberCDS, mean = 0, sd = 1),ncol=NumberCDS,nrow=NumberSimulation,byrow=FALSE)

XMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
#we impose correlation
for (i in seq(1,NumberSimulation)) {
  XMatrix_gaussian[i,] = t(A_gaussian %*% ZMatrix_gaussian[i,]) # t() in order to keep X as a row vector
}

#Use normal CDF to map to uniform vector U
UMatrix_gaussian = pnorm(XMatrix_gaussian)

TauMatrix_gaussian = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
for (i in seq(1,NumberCDS)) {
  TauMatrix_gaussian[,i] = ConvertToDefaultTime(CreditCurveCollection[[i]],UMatrix_gaussian[,i])
}

SingleName_PremiumLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
SingleName_DefaultLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)

Basket_PremiumLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)
Basket_DefaultLeg = matrix(data = NA,ncol=NumberCDS, nrow=NumberSimulation)

for (i in seq(1,NumberSimulation)) {
  nbdefault = 0
  
  for (j in seq(1,NumberCDS)) {
    if (TauMatrix_gaussian[i,j] == Inf) {
      # no default on jth CDS
      # premium leg
      if (i==1) SingleName_PremiumLeg[i,j] = Maturity
      else SingleName_PremiumLeg[i,j] = ((i-1)*SingleName_PremiumLeg[i-1,j] + Maturity)/i
      #default leg
      if (i==1) SingleName_DefaultLeg[i,j] = 0
      else SingleName_DefaultLeg[i,j] = ((i-1)*SingleName_DefaultLeg[i-1,j] + 0)/i      
    }
    else {
      # default on jth CDS
      nbdefault = nbdefault + 1
      # premium leg
      if (i==1) SingleName_PremiumLeg[i,j] = TauMatrix_gaussian[i,j]
      else SingleName_PremiumLeg[i,j] = ((i-1)*SingleName_PremiumLeg[i-1,j] + TauMatrix_gaussian[i,j])/i
      # default leg
      if (i==1) SingleName_DefaultLeg[i,j] = (1-RecoveryRate)
      else SingleName_DefaultLeg[i,j] = ((i-1)*SingleName_DefaultLeg[i-1,j] + (1-RecoveryRate))/i   
    }
  }
  
  #vector of tau sorted (increasing)
  sorted_tau = sort(TauMatrix_gaussian[i,])
  for (k in seq(1,NumberCDS)) {
    if (nbdefault > 0 && sorted_tau[k] != Inf) {
      # if there is at least 1 default (>0) and there is a kth default
      # premium leg
      if (i==1) Basket_PremiumLeg[i,k]  = sorted_tau[k] 
      else Basket_PremiumLeg[i,k] = ((i-1)*Basket_PremiumLeg[i-1,k] + sorted_tau[k])/i
      # default leg
      if (i==1) Basket_DefaultLeg[i,k]  = (1-RecoveryRate)*(1/NumberCDS)
      else Basket_DefaultLeg[i,k] = ((i-1)*Basket_DefaultLeg[i-1,k] + (1-RecoveryRate)*(1/NumberCDS))/i      
    }
    else {
      # no default
      # premium leg
      if (i==1) Basket_PremiumLeg[i,k]  = Maturity
      else Basket_PremiumLeg[i,k] = ((i-1)*Basket_PremiumLeg[i-1,k] + Maturity)/i
      # default leg
      if (i==1) Basket_DefaultLeg[i,k]  = 0
      else Basket_DefaultLeg[i,k] = ((i-1)*Basket_DefaultLeg[i-1,k] + 0)/i       
    }
  }
}

#cbind(TauMatrix_gaussian,SingleName_PremiumLeg,SingleName_DefaultLeg)
for (j in seq(1,NumberCDS)) {
  cat("CDS",j,"===>","default_leg=",SingleName_DefaultLeg[NumberSimulation,j],"premium_leg=",SingleName_PremiumLeg[NumberSimulation,j],"par_spread=",SingleName_DefaultLeg[NumberSimulation,j]/SingleName_PremiumLeg[NumberSimulation,j],"\n")
}

for (k in seq(1,NumberCDS)) {
  cat(k,"th to default basket ===>","default_leg=",Basket_DefaultLeg[NumberSimulation,k],"premium_leg=",Basket_PremiumLeg[NumberSimulation,k],"par_spread=",Basket_DefaultLeg[NumberSimulation,k]/Basket_PremiumLeg[NumberSimulation,k]*10000,"\n")
}


