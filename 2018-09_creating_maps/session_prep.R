# session_prep.R 
# Installs R packages required for this introduction to making maps in R.

requiredPackages <- c("knitr",  "tidyverse", "magrittr", "ggplot2", "maps", 
                      "maptools", "rgdal", "rgeos", 
                      "sp", "ggplot2", "leaflet", "readxl", "readr", 
                      "ggmap",  "tmap", "tmaptools" )

# Install all packages
install.packages(requiredPackages, dependencies=TRUE)


# Install packages not on system (not recommended as may have version issues)
# packages_to_install <- requiredPackages[!(requiredPackages %in% installed.packages()[,1])]
# if(length(packages_to_install)>0 ) install.packages(packages_to_install, dependencies=TRUE)

# Load
lapply(requiredPackages, library, character.only=TRUE)
