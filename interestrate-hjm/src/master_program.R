#master program. invoke all the other programs

#clear workspace
rm(list = ls())
#variables
current_dir = "P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/src"
#End of variables

print("Program starting...")
#set current directory
setwd(current_dir)

#source("class_definition.R") # define classes
#historical data loading, cleaning. Credit Curve Construction
source("market_data_functions.R") 
#source("market_data_loading.R") 
