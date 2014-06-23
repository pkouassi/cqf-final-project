NumberSimulation = 100
NumberOfTimesteps = 200
maturityBucket = 1/12
NumberOfYear = 2
timestep = 0.01
MaxMaturity = 2
MaturityList = seq(1/12,MaxMaturity,by=maturityBucket)
input_data = runif(n=length(MaturityList),min=0.4, max=0.9)

#pre-calculation
quantity_1 = rep(NA,length(MaturityList))
quantity_2 = rep(NA,length(MaturityList))
quantity_3 = rep(NA,length(MaturityList))
quantity_4 = rep(NA,length(MaturityList))
for (j in seq(1,length(MaturityList))) {
  quantity_1[j] = rnorm(n=1,mean = 0, sd = 1) * j
  quantity_2[j] = runif(n=1,min=0, max=0.5)
  quantity_3[j] = runif(n=1,min=0, max=0.5)
  quantity_4[j] = runif(n=1,min=0, max=0.5)
}

#init of random number
dX_Master = rnorm(n = NumberSimulation * 3 * NumberOfTimesteps, mean = 0, sd = 1)

timestamp = Sys.time()
#monte carlo loop

#testfunc = function(i,j) {
#  
#}


for (k in seq(1,NumberSimulation)){
  if (k%%(NumberSimulation/20) == 0) cat((k/NumberSimulation)*100,"% ...\n")
  
  #matrix init
  mat = matrix(NA, nrow=(NumberOfTimesteps+1),ncol=length(MaturityList),byrow = TRUE);
  mat[1,] = input_data
  dX = matrix(dX_Master[((k-1)* 3 * NumberOfTimesteps+1):(k * 3 * NumberOfTimesteps)],ncol=3,nrow=NumberOfTimesteps,byrow = TRUE) 
  
  #matrix population
  for (i in seq(2,nrow(mat))) {  
    for (j in seq(1,ncol(mat))) {
      if (j<ncol(mat)) {
        mat[i,j] = mat[i-1,j] + quantity_1[j]*timestep + 
          sum(quantity_2[j]*dX[i-1,1],quantity_3[j]*dX[i-1,2],quantity_4[j]*dX[i-1,3])*sqrt(timestep) +
          ((mat[i-1,j+1]-mat[i-1,j])/(maturityBucket))*timestep 
      }
      else if (j == ncol(mat)) {
        mat[i,j] = mat[i-1,j] + quantity_1[j]*timestep + 
          sum(quantity_2[j]*dX[i-1,1],quantity_3[j]*dX[i-1,2],quantity_4[j]*dX[i-1,3])*sqrt(timestep) +
          ((mat[i-1,j]-mat[i-1,j-1])/(maturityBucket))*timestep 
      }
      
    }
  }
  
  
  
  #perform calculation using data in matrix mat
  #...
}
cat("time to complete MC loop:",Sys.time()-timestamp, "\n")

