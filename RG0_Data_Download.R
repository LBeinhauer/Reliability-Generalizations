### Reliability Generalization HEXACO ###



###################################################################################################
# This script is used purely for downloading the data from the respective OSF-repositories.       #
####################################################################################################


# library loading and installing as necessary


# relevant libraries required for this script
packages <- c("magrittr", "osfr", "here")

# check, whether library already installed or not - install and load as needed:
apply(as.matrix(packages), MARGIN = 1, FUN = function(x) {
  
  pkg_avail <- nzchar(system.file(package = x))   # check if library is installed on system
  
  if(pkg_avail){
    require(x, character.only = TRUE)             # load the library, if already installed
    
  }else{
    install.packages(x)                           # install the library, if missing
    require(x, character.only = TRUE)             # load after installation
  }
})



### Many Labs 1


### Many Labs 2


### Many Labs 3

# use osfr-package to download Many Labs 3 data, on IPD-level
osfr::osf_retrieve_file("https://osf.io/bxw8j") %>%
  osfr::osf_download(path = here("Data/Downloaded Data"))
# Data is downloaded in a .zip file to the Downloaded Data sub-folder

# Unzip the zip folder and transfer the data to the Original Data sub-folder
unzip(here("Data/Downloaded Data/Ml3 Final Data.zip"),
      files = "ML3 Final Data/ML3AllSitesandmTurk.csv",
      exdir = here("Data/Original Data/ManyLabs3"))


### Many Labs 5 - Shnabel

# use osfr-package to download Many Labs 5 data, replications concerning Shnabel et al.
osfr::osf_retrieve_file("https://osf.io/da8bh") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs5/Shnabel/"))



### Many Labs 5 - Albarracin



### Many Labs 5 - Payne



### Many Labs 5 - LoBue



### RRR1



### RRR3



### RRR4



### RRR5 - Finkel

# use osfr-package to downlaod RRR5 data on the Finkel et al. replications
osfr::osf_retrieve_file("https://osf.io/dvaz7") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data"))
# again, a .zip file is stored in Downloaded Data

# unzip the file and move to corresponding directory
unzip(here("Data/Downloaded Data/All_Data.zip"),
      exdir = here("Data/Original Data/RRR5/"))



### RRR6



### RRR7



### RRR8



### RRR9



### RRR10 - Mazar

# use osfr-package to download RRR10 data on the Mazar et al. / Srull et al. replications
osfr::osf_retrieve_file("https://osf.io/fwnc2") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data/"))
# zip-file is stored at Downloaded Data

# again, unzip the file and move it to the corresponding directory
unzip(here("Data/Downloaded Data/Meta-Analysis_2018-07-09.zip"),
      files = "Meta-Analysis/Results/raw_data_corrected_MAA.csv",
      exdir = here("Data/Original Data/RRR10"))

# Now, all data should be downloaded and moved to the corresponding "Original Data" directory
# As the data standards differ across projects, in E1_Data_Extraction.R, relevant data will
#  be taken from the respective files and made ready for analysis.

