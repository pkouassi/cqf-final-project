#master program. invoke all the other programs

#variables
current_dir = "P://CQF//FinalProject//git-root//finalproject//creditbasket//src//"
#End of variables

print("Program starting...")
#set current directory
setwd(current_dir)

#historical data loading, cleaning
source("credit_data_parsing.R") #load a function
source("parse_historical_yield_curve.R") # process data
source("credit_curve_bootstrapping.R") # load functions and classes

#gaussian correlation matrix, student t correlation matrix and degree of freedom
source("default_probability_correlation_matrix.R") # load functions and process data
print(DefaultProbabilityMatrix_GaussianCopula)
print(DefaultProbabilityMatrix_KendallTau)
cat("degree of freedom",degree_freedom)

#sampling from copula
source("marginal_default_time.R") #daily cds data loading and function definition
source("kthdefault_algorithm.R") #function loading
source("sampling_from_gaussian_copula.R") #sampling from copula process
print(expectation_spread_gaussian)

#sampling from student t copula
source("sampling_from_student_t_copula.R")
print(expectation_spread_studentt)


