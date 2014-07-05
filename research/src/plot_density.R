#Make dummy data
dat <- rnorm(1000)
extra_dat <- rnorm(1000)
#Plot
plot(density(dat),col="blue")
lines(density(extra_dat),col="red")