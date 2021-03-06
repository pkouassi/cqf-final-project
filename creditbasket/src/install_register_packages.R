#==============================================================================
# title           :install_register_packages.R
# description     :Ensure that all external packages are installed and loaded
# author          :Bertrand Le Nezet
# date            :20140713
# version         :1.0    
#==============================================================================

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

package_list = c("randtoolbox")

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
