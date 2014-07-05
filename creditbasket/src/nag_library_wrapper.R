#quasirandom.nag(100,5,"sobol","C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll")

quasirandom.nag = function(n, dimension, gentype, dllpathname){
  #Fortran NAG library loading
  dyn.load(dllpathname)
  
  #initialization of the quasi-random generator
  if (gentype == "sobol") {
    genid = 1
  }
  else if (gentype == "niederreiter") {
    genid = 3
  }
  else if (gentype == "faure") {
    genid = 4
  }
  else {
    #defaut
    genid = 1
  }

  idim = dimension
  liref = 32 * idim + 7
  iref = vector("integer",liref)
  ifail = 1
  ans_init = .Fortran("G05YLF",genid=as.integer(genid),idim=as.integer(idim),iref=iref,liref=as.integer(liref),iskip=as.integer(1),ifail=as.integer(ifail))
  
  #generation of the quasi-random numbers
  xmean = rep(0,idim)
  std = rep(1,idim)
  quas = matrix(numeric(1),n,idim)
  ifail = 1
  ans_gen = .Fortran("G05YJF",xmean=xmean,std=std,n=as.integer(n),quas=quas,iref=ans_init$iref,ifail=as.integer(ifail))
  
  return(ans_gen$quas)
}