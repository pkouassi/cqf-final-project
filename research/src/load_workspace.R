loadOneName <- function(objName, file, envir = parent.frame(), 
                        assign.on.exit = TRUE) { 
  tempEnv <- new.env() 
  load(file, envir = tempEnv) 
  stopifnot(objName %in% ls(tempEnv)) 
  if(assign.on.exit) { 
    assign(objName, tempEnv[[objName]], envir = envir) 
    return(invisible(tempEnv[[objName]])) 
  } 
  tempEnv[[objName]] 
} 

install.packages("R.utils")
library("R.utils") 

saveRDS(ans24,"D:/temp/R-workspace/ans24-sobol.rds")
#test.ans24 = readRDS("D:/temp/R-workspace/ans24.rds")
saveRDS(ans25,"D:/temp/R-workspace/ans25-niederreiter.rds")
saveRDS(ans27,"D:/temp/R-workspace/ans27-rnorm.rds")

saveRDS(ans17,"D:/temp/R-workspace/ans17.rds")
saveRDS(ans18,"D:/temp/R-workspace/ans18.rds")
saveRDS(ans19,"D:/temp/R-workspace/ans19.rds")

saveRDS(ans_student1,"D:/temp/R-workspace/ans_student1.rds")
saveRDS(ans_student2,"D:/temp/R-workspace/ans_student2.rds")
saveRDS(ans_student3,"D:/temp/R-workspace/ans_student3.rds")
saveRDS(ans_student4,"D:/temp/R-workspace/ans_student4.rds")
saveRDS(ans_student5,"D:/temp/R-workspace/ans_student5.rds")
saveRDS(ans_student6,"D:/temp/R-workspace/ans_student6.rds")
saveRDS(ans_student7,"D:/temp/R-workspace/ans_student7.rds")
saveRDS(ans_student8,"D:/temp/R-workspace/ans_student8.rds")
saveRDS(ans_student9,"D:/temp/R-workspace/ans_student9.rds")
saveRDS(ans_student10,"D:/temp/R-workspace/ans_student10.rds")
saveRDS(ans_student11,"D:/temp/R-workspace/ans_student11.rds")
saveRDS(ans_student12,"D:/temp/R-workspace/ans_student12.rds")
saveRDS(ans_student13,"D:/temp/R-workspace/ans_student13.rds")
saveRDS(ans_student14,"D:/temp/R-workspace/ans_student14.rds")
saveRDS(ans_student15,"D:/temp/R-workspace/ans_student15.rds")
saveRDS(ans_student16,"D:/temp/R-workspace/ans_student16.rds")

#saveRDS(ans_hjm1,"D:/temp/R-workspace/ans_hjm1.rds")
#saveRDS(ans_hjm1bis,"D:/temp/R-workspace/ans_hjm1bis.rds")
saveRDS(ans_hjm2,"D:/temp/R-workspace/ans_hjm2.rds")
saveRDS(ans_hjm3,"D:/temp/R-workspace/ans_hjm3.rds")
saveRDS(ans_hjm4,"D:/temp/R-workspace/ans_hjm4.rds")
saveRDS(ans_hjm5,"D:/temp/R-workspace/ans_hjm5.rds")
saveRDS(ans_hjm6,"D:/temp/R-workspace/ans_hjm6.rds")
saveRDS(ans_hjm7,"D:/temp/R-workspace/ans_hjm7.rds")
saveRDS(ans_hjm8,"D:/temp/R-workspace/ans_hjm8.rds")
saveRDS(ans_hjm9,"D:/temp/R-workspace/ans_hjm9.rds")
saveRDS(ans_hjm10,"D:/temp/R-workspace/ans_hjm10.rds")
saveRDS(ans_hjm11,"D:/temp/R-workspace/ans_hjm111.rds")

ans_hjm1 = readRDS("D:/Temp/Simulation/ans_hjm1/ans_hjm1.rds")
#ans_hjm1bis = readRDS("D:/Temp/Simulation/ans_hjm1/ans_hjm1bis.rds")
ans_hjm2 = readRDS("D:/Temp/Simulation/ans_hjm1/ans_hjm2.rds")
ans_hjm3 = readRDS("D:/Temp/Simulation/ans_hjm1/ans_hjm3.rds")
ans_hjm4 = readRDS("D:/Temp/Simulation/ans_hjm1/ans_hjm4.rds")

#plot convergence for 
#1st to default
step = 10
nbobs = round(nrow(ans_hjm2$simulation)/step)
sobol = rep(NA,nbobs)
rnorm = rep(NA,nbobs)
for (i in seq(1,nbobs)) {
  sobol[i] = mean(ans_hjm2$simulation[1:i*step,52])
  rnorm[i] = mean(ans_hjm4$simulation[1:i*step,52])
}
matplot(seq(1,nbobs)*step,cbind(rnorm,sobol),type="l",log="x",col=c("red","blue"),lty = c(3,1),xlab="Number of Simulations",ylab="Cap price")
legend(460, 0.0096, c("Pseudo random number - rnorm()","Quasi random number - Sobol"), col = c("red","blue"), text.col = "black", lty = c(3,1,5),merge = TRUE, bg = "gray90")

#convergence diagram for a swap (1*5)
step = 10
nbobs = round(nrow(ans_hjm2$simulation)/step)
sobol = rep(NA,nbobs)
rnorm = rep(NA,nbobs)
for (i in seq(1,nbobs)) {
  sobol[i] = mean(ans_hjm5$simulation[1:i*step,4])
  rnorm[i] = mean(ans_hjm8$simulation[1:i*step,4])
}
matplot(seq(1,nbobs)*step,cbind(rnorm,sobol),type="l",log="x",col=c("red","blue"),lty = c(3,1),xlab="Number of Simulations",ylab="Swap price")
legend(460, 0.0544, c("Pseudo random number - rnorm()","Quasi random number - Sobol"), col = c("red","blue"), text.col = "black", lty = c(3,1,5),merge = TRUE, bg = "gray90")


#1st to default
step = 1
nbobs = round(nrow(ans1$basket_sim)/step)
sobol = rep(NA,nbobs)
niederreiter = rep(NA,nbobs)
rnorm = rep(NA,nbobs)
for (i in seq(1,nbobs)) {
  sobol[i] = ans1$basket_sim[i*step,1]/ans24$basket_sim[i*step,6]
  niederreiter[i] = ans25$basket_sim[i*step,1]/ans25$basket_sim[i*step,6]
  rnorm[i] = ans27$basket_sim[i*step,1]/ans27$basket_sim[i*step,6]
}

matplot(seq(1,nbobs)*step,cbind(rnorm,sobol,niederreiter),type="l",log="x",col=c("black","black","black"),lty = c(3,1,5),xlab="Number of Simulations",ylab="1st to default basket fair spread")
legend(10000, 0.0026, c("Pseudo random number - rnorm()","Quasi random number - Sobol","Quasi random number - Niederreiter"), col = c("black","black","black"), text.col = "black", lty = c(3,1,5),
       merge = TRUE, bg = "gray90")


ans1 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans1"]]

ans2 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans2"]]
ans3 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans3"]]
ans4 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans4"]]
ans5 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans5"]]

#plot impact of recovery rate
X = c(30,40,50,60)
Y = matrix(NA,nrow=5,ncol=4)
Y[,1] = ans17$basket_spreads
Y[,2] = ans3$basket_spreads
Y[,3] = ans18$basket_spreads
Y[,4] = ans19$basket_spreads
matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Recovery rate (%)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5))
legend(53.5, 96, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"),       text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),
       merge = TRUE, bg = "gray90")


X = c(50,100,250,500)
Y = matrix(NA,nrow=5,ncol=4)
Y[,1] = ans2$basket_spreads
Y[,2] = ans3$basket_spreads
Y[,3] = ans4$basket_spreads
Y[,4] = ans5$basket_spreads
matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Single-name spread (bp)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5))
legend(45, 350, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"),       text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),
       merge = TRUE, bg = "gray90")

ans6 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans6"]]
ans7 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans7"]]
ans8 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans8"]]
ans9 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans9"]]
ans10 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans10"]]
ans11 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans11"]]
ans12 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans12"]]
ans13 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans13"]]
ans14 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans14"]]
ans15 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans15"]]
ans16 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans16"]]

ans24 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans24"]]

attributes(ans24)

Y = ans24$basket_sim[,1]/ans24$basket_sim[,6]

#1st to default
# step = 100
# nbobs = round(nrow(ans24$basket_sim)/step)
# sobol = rep(NA,nbobs)
# niederreiter = rep(NA,nbobs)
# rnorm = rep(NA,nbobs)
# for (i in seq(1,nbobs)) {
#   sobol[i] = ans24$basket_sim[i*step,1]/ans24$basket_sim[i*step,6]
#   niederreiter[i] = ans25$basket_sim[i*step,1]/ans25$basket_sim[i*step,6]
#   rnorm[i] = ans27$basket_sim[i*step,1]/ans27$basket_sim[i*step,6]
# }
# 
# matplot(seq(1,nbobs)*step,cbind(rnorm,sobol,niederreiter),type="l",log="x",col=c("black","black","black"),lty = c(3,1,5),xlab="Number of Simulations",ylab="1st to default basket fair spread")
# legend(10000, 0.0026, c("Pseudo random number - rnorm()","Quasi random number - Sobol","Quasi random number - Niederreiter"), col = c("black","black","black"), text.col = "black", lty = c(3,1,5),
#        merge = TRUE, bg = "gray90")

#close-up
# step = 1000
# nbobs = round(nrow(ans24$basket_sim)/step)
# sobol = rep(NA,nbobs)
# niederreiter = rep(NA,nbobs)
# rnorm = rep(NA,nbobs)
# start_index = round(nbobs*2/3)
# for (i in seq(1,nbobs)) {
#   sobol[i] = ans24$basket_sim[i*step,1]/ans24$basket_sim[i*step,6]
#   niederreiter[i] = ans25$basket_sim[i*step,1]/ans25$basket_sim[i*step,6]
#   rnorm[i] = ans27$basket_sim[i*step,1]/ans27$basket_sim[i*step,6]
# }
# matplot(seq(start_index,nbobs)*step,cbind(sobol[start_index:nbobs],niederreiter[start_index:nbobs]),type
#         ="l",col=c("black","black","black"),lty = c(3,1,5),xlab="Number of Simulations",ylab="1st to default basket fair spread")

#2nd to default
# step = 100
# nbobs = round(nrow(ans24$basket_sim)/step)
# sobol = rep(NA,nbobs)
# niederreiter = rep(NA,nbobs)
# rnorm = rep(NA,nbobs)
# for (i in seq(1,nbobs)) {
#   sobol[i] = ans24$basket_sim[i*step,2]/ans24$basket_sim[i*step,7]
#   niederreiter[i] = ans25$basket_sim[i*step,2]/ans25$basket_sim[i*step,7]
#   rnorm[i] = ans27$basket_sim[i*step,2]/ans27$basket_sim[i*step,7]
# }
# 
# matplot(seq(1,nbobs)*step,cbind(rnorm,sobol,niederreiter),type="l",log="x",col=c("black","black","black"),lty = c(3,1,5),xlab="Number of Simulations",ylab="1st to default basket fair spread")
# legend(10000, 0.0026, c("Pseudo random number - rnorm()","Quasi random number - Sobol","Quasi random number - Niederreiter"), col = c("black","black","black"), text.col = "black", lty = c(3,1,5),
#        merge = TRUE, bg = "gray90")

ans6$basket_spreads

X = c(seq(0,90,by=10),99)
Y = matrix(NA,nrow=5,ncol=11)
Y[,1] = ans6$basket_spreads
Y[,2] = ans7$basket_spreads
Y[,3] = ans8$basket_spreads
Y[,4] = ans9$basket_spreads
Y[,5] = ans10$basket_spreads
Y[,6] = ans11$basket_spreads
Y[,7] = ans12$basket_spreads
Y[,8] = ans13$basket_spreads
Y[,9] = ans14$basket_spreads
Y[,10] = ans15$basket_spreads
Y[,11] = ans16$basket_spreads
matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Correlation (%)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5))
legend(75, 100, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"),       text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),
       merge = TRUE, bg = "gray90")

ans20 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans20"]]
ans21 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans21"]]
ans22 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans22"]]
ans23 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans23"]]

X = c(1,2,3,5)
Y = matrix(NA,nrow=5,ncol=4)
Y[,1] = ans20$basket_spreads
Y[,2] = ans21$basket_spreads
Y[,3] = ans22$basket_spreads
Y[,4] = ans23$basket_spreads
matplot(X,t(Y),type="b",ylab="Basket spread (bp)",xlab="Correlation (%)", col="black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5), log="x")
legend(75, 100, c("1st to default","2nd to default","3rd to default","4th to default","5th to default"), col = c("black"),       text.col = "black", lty = c(1,2,3,4,5), pch = c(1,2,3,4,5),
       merge = TRUE, bg = "gray90")


