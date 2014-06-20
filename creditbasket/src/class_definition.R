#YielcCurve
setClass("YieldCurve",
         representation(
           time = "vector",
           discountfactor = "vector")
)

#CreditDefaultSwap
setClass("CreditDefaultSwap", 
         representation(
           maturity = "numeric", 
           marketprice = "numeric" #market spread
         )
)

#CreditCurve
setClass("CreditCurve",
         representation(
           time = "vector",
           spread = "vector",
           survivalprobability = "vector",
           hazardrate = "vector")
)