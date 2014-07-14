is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

package_list = c("randtoolbox","doParallel","plyr","foreach","compiler")

for (i in seq(1,length(package_list))) {
  package = package_list[i]
  
  if (is.installed(package)) {
    cat("[",package,"]","package is already installed\n")
    library(package,character.only = TRUE)
    cat("[",package,"]","package is registered\n")
  }
  else {
    install.packages(package)
    cat("[",package,"]","package is now installed\n")
    library(package,character.only = TRUE)
    cat("[",package,"]","package is registered\n")
  }
}

#enableJIT(1)