#Sampling from gaussian copula

ans1 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                  CDS2_USD_XR_MARGINAL_CreditCurve,
                                  CDS3_USD_XR_MARGINAL_CreditCurve,
                                  CDS4_USD_XR_MARGINAL_CreditCurve,
                                  CDS5_USD_XR_MARGINAL_CreditCurve),
                                  YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-sobol")


#first to default
#plot(seq(100,100000),ans$basket_sim[100:100000,1]/ans$basket_sim[100:100000,6],type="l",log="x")

#second to default
#plot(seq(100,100000),ans$basket_sim[100:100000,2]/ans$basket_sim[100:100000,7],type="l",log="x")

#third to default
#plot(seq(100,100000),ans$basket_sim[100:100000,3]/ans$basket_sim[100:100000,8],type="l",log="x")


#Risk and sensitivity analysis

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_50 = GetFlatCreditCurve(50,yieldcurve_flat)
ans2 = BasketCDSPricing_GaussianCopula(c(cc_50,cc_50,cc_50,cc_50,cc_50),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans3 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

cc_250 = GetFlatCreditCurve(250,yieldcurve_flat)
ans4 = BasketCDSPricing_GaussianCopula(c(cc_250,cc_250,cc_250,cc_250,cc_250),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

cc_500 = GetFlatCreditCurve(500,yieldcurve_flat)
ans5 = BasketCDSPricing_GaussianCopula(c(cc_500,cc_500,cc_500,cc_500,cc_500),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")

ans6 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.0,5),0.40,1000000,"nag-sobol")
ans7 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.1,5),0.40,1000000,"nag-sobol")
ans8 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.2,5),0.40,1000000,"nag-sobol")
ans9 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,1000000,"nag-sobol")
ans10 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.4,5),0.40,1000000,"nag-sobol")
ans11 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.5,5),0.40,1000000,"nag-sobol")
ans12 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.6,5),0.40,1000000,"nag-sobol")
ans13 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.7,5),0.40,1000000,"nag-sobol")
ans14 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.8,5),0.40,1000000,"nag-sobol")
ans15 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.9,5),0.40,1000000,"nag-sobol")
ans16 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.99,5),0.40,1000000,"nag-sobol")

#to be re-run
ans17 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.30,1000000,"nag-sobol")
ans18 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.50,1000000,"nag-sobol")
ans19 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.60,1000000,"nag-sobol")
#-------------

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans20 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.02*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans21 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.03*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans22 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.05*x)))
cc_100 = GetFlatCreditCurve(50,yieldcurve_flat)
ans23 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,1000000,"nag-sobol")

#to be re-run
#sobol / convergence
ans24 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-sobol")

ans25 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                          YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-niederreiter")

ans27 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"rnorm")

ans26 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-faure")


yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.01*x)))
cc_entity1 = GetFlatCreditCurve(20.50,yieldcurve_flat)
cc_entity2 = GetFlatCreditCurve(20.6150,yieldcurve_flat)
cc_entity3 = GetFlatCreditCurve(20,yieldcurve_flat)

ans27 = BasketCDSPricing_GaussianCopula(c(cc_entity1,
                                          cc_entity2,
                                          cc_entity3),
                                       yieldcurve_flat,UniformCorrelationMatrix(0.30,3),0.40,10000,"sobol")


#ans$basket_spreads
#ans$singlename_spreads
#ans$basket_sim
#ans$singlename_sim
#ans$tau

#rnorm vs. sobol
#first_to_default.rnorm = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#first_to_default.sobol = ans$basket_sim[250:100000,1]/ans$basket_sim[250:100000,6]
#matplot(seq(250,100000),cbind(first_to_default.rnorm,first_to_default.sobol),type="l",log="x")

#new batch to run


ans24 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-sobol")

ans25 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"nag-niederreiter")

ans27 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"rnorm")

ans28 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,1000000,"halton")

ans29 = BasketCDSPricing_GaussianCopula(c(CDS1_USD_XR_MARGINAL_CreditCurve,
                                          CDS2_USD_XR_MARGINAL_CreditCurve,
                                          CDS3_USD_XR_MARGINAL_CreditCurve,
                                          CDS4_USD_XR_MARGINAL_CreditCurve,
                                          CDS5_USD_XR_MARGINAL_CreditCurve),
                                        YieldCurve,CorrelationMatrix_GaussianCopula,0.40,100,"torus")

#to re-run
nbsim=100
yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.001*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans30 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.005*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans31 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.010*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans32 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.015*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans33 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.020*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans34 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.025*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans35 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.030*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans36 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.035*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans37 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.040*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans38 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.045*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans39 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")

yieldcurve_flat = new ("YieldCurve", time = c(1,2,3,4,5), discountfactor = sapply(seq(1,5),function (x) exp(-0.050*x)))
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)
ans40 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.30,5),0.40,nbsim,"nag-sobol")


X = c(0.1,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
Y = matrix(NA,nrow=5,ncol=length(X))
Y[,1] = ans30$basket_spreads
Y[,2] = ans31$basket_spreads
Y[,3] = ans32$basket_spreads
Y[,4] = ans33$basket_spreads
Y[,5] = ans34$basket_spreads
Y[,6] = ans35$basket_spreads
Y[,7] = ans36$basket_spreads
Y[,8] = ans37$basket_spreads
Y[,9] = ans38$basket_spreads
Y[,10] = ans39$basket_spreads
Y[,11] = ans40$basket_spreads

matplot(X,t(Y)[,1],type="b",ylab="Basket spread (bp)",xlab="Interest rate (%)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5))

####---------
nbsim=100
cc_100 = GetFlatCreditCurve(100,yieldcurve_flat)

ans41 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.10,nbsim,"nag-sobol")

ans42 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.20,nbsim,"nag-sobol")

ans43 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.30,nbsim,"nag-sobol")

ans44 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.40,nbsim,"nag-sobol")

ans45 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.50,nbsim,"nag-sobol")

ans46 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.60,nbsim,"nag-sobol")

ans47 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.70,nbsim,"nag-sobol")

ans48 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.80,nbsim,"nag-sobol")

ans49 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.90,nbsim,"nag-sobol")

ans50 = BasketCDSPricing_GaussianCopula(c(cc_100,cc_100,cc_100,cc_100,cc_100),yieldcurve_flat,UniformCorrelationMatrix(0.3,5),0.99,nbsim,"nag-sobol")

X = c(10,20,30,40,50,60,70,80,90,99)
Y = matrix(NA,nrow=5,ncol=length(X))
Y[,1] = ans41$basket_spreads
Y[,2] = ans42$basket_spreads
Y[,3] = ans43$basket_spreads
Y[,4] = ans44$basket_spreads
Y[,5] = ans45$basket_spreads
Y[,6] = ans46$basket_spreads
Y[,7] = ans47$basket_spreads
Y[,8] = ans48$basket_spreads
Y[,9] = ans49$basket_spreads
Y[,10] = ans50$basket_spreads
matplot(X,t(Y)[,1:2],type="b",ylab="Basket spread (bp)",xlab="Recovery rate (%)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5))
