#==============================================================================
# title           :class_definition.R
# description     :Defines common class
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# YielcCurve class
setClass("YieldCurve",
         representation(
           time = "vector",
           discountfactor = "vector")
)