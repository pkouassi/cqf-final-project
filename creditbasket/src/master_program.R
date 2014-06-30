#master program. invoke all the other programs

#clear workspace
rm(list = ls())
#variables
current_dir = "P:/CQF/FinalProject/git-root/finalproject/creditbasket/src"
#End of variables

print("Program starting...")
#set current directory
setwd(current_dir)

source("class_definition.R") # define classes
#install / load dependencies
source("install_register_packages.R") 
#historical data loading, cleaning. Credit Curve Construction
source("market_data_functions.R") 
source("market_data_loading.R") 
source("credit_curve_bootstrapping_functions.R")

#gaussian correlation matrix, student t correlation matrix and degree of freedom
source("default_probability_correlation_matrix.R") # load functions and process data
print(DefaultProbabilityMatrix_GaussianCopula)
print(DefaultProbabilityMatrix_KendallTau)
print(DefaultProbabilityMatrix_StudentTCopula)
cat("degree of freedom",degree_freedom)

#Load NAG library
source("nag_library_wrapper.R")

#sampling from copula
source("marginal_default_time.R") #daily cds data loading and function definition
source("kthdefault_algorithm.R") #function loading
source("sampling_from_gaussian_copula.R") #sampling from copula process
print(expectation_spread_gaussian)

#sampling from student t copula
source("sampling_from_student_t_copula.R")
print(expectation_spread_studentt)


