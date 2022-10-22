
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
  # the team that provides you data will probably have done 
  # some ETL on it. We'll call this the "initial" ETL
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


# The team that provided you data did a great job, but `pointblank` 
# revealed there are some things about the data that still need to be done. 
# 
# Let's do ETL based on the results of our `pointblank` assessment. We'll call this
# `pointblanked` ETL. 



# Now that we've done `pointblanked` ETL, let's do the same analysis again



# Interesting, our findings are different! 





