## Advanced R day 4 - IMD R training


# Install and Load Packages
packages <- c("devtools", "usethis", "roxygen2", "stringr", "dplyr", "ggwordcloud")

installed_packages <- packages %in% installed.packages() # check which packages are installed
if (length(packages[!installed_packages]) > 0){
  install.packages(packages[!installed_packages], dep = TRUE)} # if some are missing, install them

# Load Packages
lapply(packages, library, character.only = TRUE) 

#Set Working Directory
setwd("D:/AAAA_MJJ/AAAA Monitoring/Breeding Bird/R Analysis/CODE/NCRNbirds/Data/IMD2022/data")

# used to set up github token
usethis::create_github_token()

#used to paste in your github token
gitcreds::gitcreds_set()




