install.packages("plot3D")
library(plot3D)

install.packages("rgl")
library(rgl)

open3d()
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)
plot3d(x, y, z, col=rainbow(1000))

surf(CapVolSurfaceIVsMatrix)

?persp 

persp(seq(10, 300, 5), seq(10, 300, 5), matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3), phi = 45, theta = 45, xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",main = "Surface elevation data").


x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
z <- outer(x, y, function(a, b) a*b^2)
persp(x, y, z)

x = CapVolSurfaceStrikes*100
y = CapVolSurfaceMaturities
z = CapVolSurfaceIVsMatrix*100
persp(CapVolSurfaceStrikes*100, CapVolSurfaceMaturities, CapVolSurfaceIVsMatrix*100 ,phi = 10, theta = 45, box = TRUE,  col = "lightblue",ticktype="detailed",nticks=4,shade=0.5, xlab="Strike",ylab="Maturity",zlab="Volatility")


?persp

#------------------
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
op <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",)


#-----------------
x <- seq(-pi, pi, len = 20)
y <- seq(-pi, pi, len = 20)
g <- expand.grid(x = x, y = y)
g$z <- sin(sqrt(g$x^2 + g$y^2))
wireframe(z ~ x * y, g, drape = TRUE, aspect = c(3,1), colorkey = TRUE)