#==============================================================================
# title           :master_program.R
# description     :This script will invoke all the other R scipts
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# clear workspace
rm(list = ls())
# variables
current_dir = "P:/CQF/FinalProject/git-root/finalproject/creditbasket/src"
#End of variables

print("Program starting...")
#set current directory
setwd(current_dir)

# define common classes
source("class_definition.R") 
# install / load dependencies
source("install_register_packages.R") 
# historical data loading, cleaning. Credit Curve Construction
source("market_data_functions.R") 
source("market_data_loading.R") 
source("credit_curve_bootstrapping_functions.R")

# gaussian correlation matrix, student t correlation matrix and degree of freedom
source("default_probability_correlation_matrix.R") 
print(CorrelationMatrix_GaussianCopula)
print(CorrelationMatrix_KendallTau)
print(CorrelationMatrix_StudentTCopula)
cat("degree of freedom",degree_freedom)

# calculate correlation matrix using stock log return
source("stock_log_return_correlation_matrix.R")
print(StockLogReturnCorrelationMatrix)

# Estimate Marginal Hazard Rates (empirical marginal distributions)
source("estimate_marginal_hazard_rates.R")
print(HazardRatesMatrix)

# Load NAG library
source("nag_library_wrapper.R")

# Load functions used in Monte Carlo loop
source("monte_carlo_functions.R")

# sampling from gaussian copula
source("gaussian_copula_functions.R")
source("sampling_from_gaussian_copula.R")

# sampling from student t copula
source("student_t_copula_functions.R")
source("sampling_from_student_t_copula.R")



