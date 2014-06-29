#Calling NAG fortran library (64 bit version) from R 64 bits

#loading dll
dyn.load("C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")

#
is.loaded("A00AAF") #returns TRUE
.Fortran("A00AAF")
#returns the following
#*** Start of NAG Library implementation details ***
#  
#  Implementation title: Microsoft Windows x64, Intel Fortran
#Precision: FORTRAN double precision
#Product Code: FLW6I24DCL
#Mark: 24.0 (self-contained)
#
#*** End of NAG Library implementation details ***

#license
is.loaded("A00ACF") #returns TRUE
.Fortran("A00ACF") #returns list(), i would have expected more

#machine precision
is.loaded("X02AJF") #returns TRUE
.Fortran("X02AJF",X=integer(1)) #returns list(), i would have expected more

#Bessel function 
is.loaded("S17AEF")
x = as.numeric(1)
ifail = as.integer(0)
y = .Fortran("S17AEF",x=2,ifail=1)
y
#returns
#$x
#[1] 2
#
#$ifail
#[1] 1

#working code with NAG
x <- c(0.5,1.0,3.0,6.0,8.0,10.0,1000.0)
ifail <- 0
ans <- .Fortran("S17AQF",n=as.integer(length(x)),x=x,f=vector("double",length(x)),ivalid=vector("integer",length(x)),ifail=as.integer(ifail))
ans$f

#------------------------
#1D
#------------------------


#init working
dyn.load("C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
liref = 32 * 1 + 7
iref = vector("integer",liref)
ifail = 1
ans = .Fortran("G05YLF",genid=as.integer(1),idim=as.integer(1),iref=iref,liref=as.integer(liref),iskip=as.integer(1),ifail=as.integer(ifail))

#generation working
#G05YJF(mu, sigma, n, Z(1), IREF(1), ifail)
mu = 0
sigma = 1
n = 50
Zvector = vector("numeric",n) #or vector of double
iref=ans$iref #read iref coming from generator
ifail = 1
.Fortran("G05YJF",mu=as.numeric(mu),sigma=as.numeric(sigma),n=as.integer(n),quasi=Zvector,iref=iref,ifail=as.integer(ifail))



#------------------------
#Multi Demension
#------------------------

dyn.load("C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")
genid = 1 #we want the sobol generator
idim = 20
liref = 32 * idim + 7
iref = vector("integer",liref)
ifail = 1
ans = .Fortran("G05YLF",genid=as.integer(2),idim=as.integer(idim),iref=iref,liref=as.integer(liref),iskip=as.integer(1),ifail=as.integer(ifail))

#G05YJF(mu, sigma, n, Z(1), IREF(1), ifail)
n = 200
xmean = rep(0,idim)
std = rep(1,idim)
quas = matrix(numeric(1),n,idim)
ifail = 1
ans2 = .Fortran("G05YJF",xmean=xmean,std=std,n=as.integer(n),quas=quas,iref=ans$iref,ifail=as.integer(ifail))

ans2$quas[1,]

