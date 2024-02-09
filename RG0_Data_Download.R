### Reliability Generalization HEXACO ###



###################################################################################################
# This script is used purely for downloading the data from the respective OSF-repositories.       #
####################################################################################################


# library loading and installing as necessary


# relevant libraries required for this script
packages <- c("magrittr", "osfr", "here", "zip", "data.table")

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

# download the zipped-directory of Many Labs 1 data, from the OSF
osfr::osf_retrieve_file("https://osf.io/nqg97") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data"), conflicts = "overwrite")
# the data-file is named very generically "Datasets.zip" - it is renamed here to avoid confusion
file.rename(here("Data/Downloaded Data/Datasets.zip"), here("Data/Downloaded Data/ML1_Datasets.zip"))

# unzip the directory and extract only the specific data-file required for anayses
unzip(here("Data/Downloaded Data/ML1_Datasets.zip"),
      files = "Data/CleanedDataset.sav",
      exdir = here("Data/Original Data/ManyLabs1"),
      junkpaths = TRUE)

### Many Labs 2

# use osfr-package to download Many Labs 2 data, on IPD-level
# Many Labs 2 data comes in two separate data-files, containing data on two "slates" (projects were
#  distributed across slates). These are ML2_S1.csv (slate 1) and ML2_S2.csv (slate 2)
osfr::osf_retrieve_file("https://osf.io/cwjp3") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs2"), conflicts = "overwrite")
osfr::osf_retrieve_file("https://osf.io/jg9hc") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs2"), conflicts = "overwrite")

### Many Labs 3

# use osfr-package to download Many Labs 3 data, on IPD-level
osfr::osf_retrieve_file("https://osf.io/bxw8j") %>%
  osfr::osf_download(path = here("Data/Downloaded Data"), conflicts = "overwrite")
# Data is downloaded in a .zip file to the Downloaded Data sub-folder

# Unzip the zip folder and transfer the data to the Original Data sub-folder
unzip(here("Data/Downloaded Data/Ml3 Final Data.zip"),
      files = "ML3 Final Data/ML3AllSitesandmTurk.csv",
      exdir = here("Data/Original Data/ManyLabs3"),
      junkpaths = TRUE)


### Many Labs 5 - Shnabel

# use osfr-package to download Many Labs 5 data, replications concerning Shnabel et al.
osfr::osf_retrieve_file("https://osf.io/da8bh") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs5/Shnabel/"), conflicts = "overwrite")



### Many Labs 5 - Albarracin

# use osfr-package to download Many Labs 5 data, replications concerning Albarracin et al.
# The albarracin replication data comes in two different data-files. The first data-file contains 
#  replications obtained using a revised protocol (ML5 Alb 5 Revised Protocol In Lab.xlsx), while the 
#  second data-file contains replications obtained using the original RPP protocol (ML5 Alb 5 RPP 
#  Protocol mTurk.xlsx).
osfr::osf_retrieve_file("https://osf.io/jzdap") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs5/Albarracin/"), conflicts = "overwrite")
osfr::osf_retrieve_file("https://osf.io/946en") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs5/Albarracin/"), conflicts = "overwrite")




### Many Labs 5 - LoBue

# use osfr-package to download Many Labs 5 data, replications concerning LoBue et al.
osfr::osf_retrieve_file("https://osf.io/w7ft9") %>% 
  osfr::osf_download(path = here("Data/Original Data/ManyLabs5/LoBue/"), conflicts = "overwrite")




### RRR3 - Hart

# Use osfr-package to download RRR3 data, replications concerning Hart & Albarracin
# For RRR3, there is no single data-file containing th replication data. Instead, (among other files)
#  several SPSS .sav files are downloaded, each containing the replication data or a single lab.
osfr::osf_retrieve_node("https://osf.io/hx7a4/") %>% 
  osfr::osf_ls_files(n_max = Inf) %>% 
  osfr::osf_download(path = here("Data/Original Data/RRR3"), conflicts = "overwrite",
                     recurse = TRUE)



### RRR5 - Finkel

# use osfr-package to downlaod RRR5 data on the Finkel et al. replications
osfr::osf_retrieve_file("https://osf.io/dvaz7") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data"), conflicts = "overwrite")
# again, a .zip file is stored in Downloaded Data

# unzip the file and move to corresponding directory
unzip(here("Data/Downloaded Data/All_Data.zip"),
      exdir = here("Data/Original Data/RRR5/"))



### RRR6 - Strack

# use osfr-package to downlaod RRR5 data on the Strack et al. replications
osfr::osf_retrieve_file("https://osf.io/9j72u") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data"), conflicts = "overwrite")
# again, a .zip file is stored in Downloaded Data
# the data-file is named very generically "Datasets.zip" - it is renamed here to avoid confusion
file.rename(here("Data/Downloaded Data/Data_FINAL.zip"), here("Data/Downloaded Data/RRR6_Data_FINAL.zip"))

# unzip the file and move to corresponding directory
zip::unzip(here("Data/Downloaded Data/RRR6_Data_FINAL.zip"),
           exdir = here("Data/Original Data/RRR6/"),
           junkpaths = TRUE)

# one of the files in the zipped directory contained the special character "Ö", which could not be
#  read by the function. The character was changed to �, which we handedly rename here
file.rename(here("Data/Original Data/RRR6/�zdogru_Data.csv"), here("Data/Original Data/RRR6/Özdogru_Data.csv"))

# For this data-file, we need some additional analysis & data manipulation scripts, supplied by the
#  original authors on the OSF:
osfr::osf_retrieve_file("https://osf.io/tbq26") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data"), conflicts = "overwrite")
file.rename(here("Data/Downloaded Data/Analysis_R_Scripts.zip"), here("Data/Downloaded Data/RRR6_Analysis_R_Scripts.zip"))

# Unzip the scripts
zip::unzip(here("Data/Downloaded Data/RRR6_Analysis_R_Scripts.zip"),
           exdir = here("Data/Original Data/RRR6/"),
           junkpaths = TRUE)



### RRR8 - Dijksterhuis

# use osfr-package to downlaod RRR8 data on the Dijksterhuis et al. replications
osfr::osf_retrieve_file("https://osf.io/p2myk") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data"), conflicts = "overwrite")
# again, a .zip file is stored in Downloaded Data
# the data-file is named very generically "Datasets.zip" - it is renamed here to avoid confusion
file.rename(here("Data/Downloaded Data/Meta-analysis_final.zip"), here("Data/Downloaded Data/RRR8_Meta-analysis_final.zip"))

# unzip the file and move to corresponding directory
zip::unzip(here("Data/Downloaded Data/RRR8_Meta-analysis_final.zip"),
           exdir = here("Data/Original Data/RRR8/"),
           junkpaths = FALSE)
# For RRR8, again, multiple files (even multiple files per replication) are downloaded. To simplify
#  the download, everything, even non-required files are downloaded. The essential data lies in the
#  directories within Meta-analysis, which is further manipulated in RG1_Data_Extraction.R

# Additionally, in one of the lab-directories, there seems to be an erroneous data.file, which we remove here
#  by hand, to enable further analyses down the line.
file.remove("C:/Users/Lukas/Documents/Git_RStudio/Reliability-Generalizations/Data/Original Data/RRR8/Meta-analysis/Krahmer_data/Rentzelas_data_complete.csv")



### RRR9 - Srull & Weyer

# use osfr-package to download RRR9 data on the  Mazar et al. / Srull et al. replications
osfr::osf_retrieve_file("https://osf.io/qegfd") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data/"), conflicts = "overwrite")
# zip-file is stored at Downloaded Data

# again, unzip the file and move it to the corresponding directory
unzip(here("Data/Downloaded Data/SW_Script_and_Data.zip"),
      exdir = here("Data/Original Data/RRR9"))

# Unfortunately, in RRR9 one of the data-files (Vanpaemel) consists of a larger number of emptly columns,
#  which complicate further analyses. Therefore, these columns are removed and the data-file is replaced here
RRR9_Vanpaemel <- read.csv("C:/Users/Lukas/Documents/Git_RStudio/Reliability-Generalizations/Data/Original Data/RRR9/SW_Script_and_Data/Vanpaemel_Final.csv")
RRR9_Vanpaemel <- RRR9_Vanpaemel[,names(RRR9_Vanpaemel)[!names(RRR9_Vanpaemel) %in% names(RRR9_Vanpaemel)[grep("^X", names(RRR9_Vanpaemel))]]]
write.csv(RRR9_Vanpaemel, "C:/Users/Lukas/Documents/Git_RStudio/Reliability-Generalizations/Data/Original Data/RRR9/SW_Script_and_Data/Vanpaemel_Final.csv",
          row.names = FALSE)

### RRR10 - Mazar et al.

# use osfr-package to download RRR10 data on the Mazar et al. / Srull et al. replications
osfr::osf_retrieve_file("https://osf.io/fwnc2") %>% 
  osfr::osf_download(path = here("Data/Downloaded Data/"), conflicts = "overwrite")
# zip-file is stored at Downloaded Data

# again, unzip the file and move it to the corresponding directory
unzip(here("Data/Downloaded Data/Meta-Analysis_2018-07-09.zip"),
      files = "Meta-Analysis/Results/raw_data_corrected_MAA.csv",
      exdir = here("Data/Original Data/RRR10"),
      junkpaths = T)

# Now, all data should be downloaded and moved to the corresponding "Original Data" directory
# As the data standards differ across projects, in E1_Data_Extraction.R, relevant data will
#  be taken from the respective files and made ready for analysis.

