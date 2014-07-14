#==============================================================================
# title           :master_program.R
# description     :This script will invoke all the other R scipts
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

#clear workspace
rm(list = ls())
#variables
current_dir = "P:/CQF/FinalProject/git-root/finalproject/interestrate-hjm/src"
#End of variables

print("Program starting...")
#set current directory
setwd(current_dir)

# define classes
source("class_definition.R") 
#install / load dependencies
source("install_register_packages.R") 
#historical data loading, cleaning. Credit Curve Construction
source("market_data_functions.R") 
source("market_data_loading.R") 

#Principal Component Analysis
source("jacobi_transformation_functions.R") 
source("principal_component_analysis.R") 

#Monte Carlo Simulation / Pricing
source("black76.R")
source("nag_library_wrapper.R")
source("monte_carlo_simulation_functions.R") 
source("monte_carlo_simulation.R") 

