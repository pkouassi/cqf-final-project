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


ans1 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans1"]]

ans2 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans2"]]
ans3 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans3"]]
ans4 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans4"]]
ans5 <- loadToEnv("D:/temp/R-workspace/gausian_simulation.RData")[["ans5"]]

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


