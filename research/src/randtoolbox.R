install.packages("randtoolbox")
library(randtoolbox)


halton(1000, dim = 5, init = TRUE, normal = TRUE, usetime = FALSE)
sobol(1000, dim = 5, normal = TRUE, scrambling = 3)
torus(1000,dim=10,normal = TRUE)

