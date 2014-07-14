#==============================================================================
# title           :nag_library_wrapper.R
# description     :define a wrapper around some NAG 64bit library functions
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

# NAG Fortran library 64 bit (flw6i24dcl) needs to be installed in order to use these functions
# On the development machine, NAG Fortran library 64 bit is installed at this path:
# C://Program Files//NAG//FL24//flw6i24dcl//bin//FLW6I24DC_nag.dll"


quasirandom.nag = function(n, dimension, gentype, dllpathname){
  #Fortran NAG library loading
  dyn.load(dllpathname)
  
  #initialization of the low-discrepancy number generator
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
  
  #generation of the low-discrepancy numbers
  xmean = rep(0,idim)
  std = rep(1,idim)
  quas = matrix(numeric(1),n,idim)
  ifail = 1
  ans_gen = .Fortran("G05YJF",xmean=xmean,std=std,n=as.integer(n),quas=quas,iref=ans_init$iref,ifail=as.integer(ifail))
  
  return(ans_gen$quas)
}