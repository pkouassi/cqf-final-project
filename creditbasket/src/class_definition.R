#==============================================================================
# title           :class_definition.R
# description     :Defines common classes
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

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