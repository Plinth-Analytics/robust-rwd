
# This script will make sure we have the right packages installed. 
# You may see a warning if your version of R is different--this will 
# probably not make a difference to your findings, but if anything is 
# particularly concerning then consider look here to revise
source("setup/packages.R")

# Note: 
# * It's unfortunately necessary to download files before reading them 
#   rather than reading from the url since they're zips, remotely reading 
#   which isn't supported
# * This script is not lazy--it will download the data regardless of whether
#   any files are present, and will overwrite existing files
source("setup/download.R")

library(tidyverse)
library(pointblank)
devtools::load_all("robustrwd")

tables <- 
  read_folder_csv_zips("data") %>% 
  # see robustrwd/R/inital-etl.R for this ETL
  initial_etl()

bene <- tables$bene08

# do some analysis, get findings

# for time-to-event analyses, assume patients were diagnosed with conditions at age 65



# It's great that we have results. Still, let's run pointblank to be sure they come from
# acceptable data...

# see codebook at https://www.cms.gov/files/document/de-10-codebook.pdf-0
# pointblank agent will address the following points from the codebook: 
#  * There are 2,326,856 valid values of DESYNPUF_ID
#  * 


# do better ETL



# do analysis again, get different findings





