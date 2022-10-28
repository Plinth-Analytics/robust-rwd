## code to prepare `DATASET` dataset goes here


# Download daata from https://www.cms.gov --------------------------------------

#!/usr/bin/Rscript

library(purrr)
library(dplyr)
library(lubridate)

downloads_url <- function(which_file) {
  paste(
    "https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/Downloads/",
    which_file,
    sep = ""
  )
}

destination <- function(nm) file.path("data", paste(nm, ".csv.zip", sep = ""))

if (!dir.exists(here::here("data"))) dir.create("data")

iwalk(
  c(
    "bene08" = "DE1_0_2008_Beneficiary_Summary_File_Sample_1.zip",
    "bene09" = "DE1_0_2009_Beneficiary_Summary_File_Sample_1.zip",
    "bene10" = "DE1_0_2010_Beneficiary_Summary_File_Sample_20.zip",
    "inpatient" = "DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.zip",
    # notably missing carrier claims--not terrible for our purposes
    "outpatient" = "DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.zip"
  ),
  ~ download.file(url = downloads_url(.x), destfile = destination(.y))
)

download.file(
  url = "http://downloads.cms.gov/files/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_1.zip",
  destfile = destination("prescription")
)


# Read in files ----------------------------------------------------------------

tables_raw <-
  read_folder_csv_zips("data")

# Select 10,000 random patients

set.seed(100)

patients_n <- 10000

patients_selected <- tables_raw$bene08 %>%
  slice_sample(n = patients_n) %>%
  pull(DESYNPUF_ID) %>%
  unique()

# Data 01 ======================================================================

tables_01 <- tables_raw %>%
  purrr::map(.f = ~filter(.x, DESYNPUF_ID %in% patients_selected))

patients_01 <- tables_01$bene08
inpatient_01 <- tables_01$inpatient
outpatient_01 <- tables_01$outpatient
prescription_01 <- tables_01$prescription

# remove prescription info from after 2008

prescription_01 <- prescription_01 %>%
  filter(SRVC_DT <= 20090000)

# Inject problems ---------------------------------------------------------

patients_01 <- patients_01 %>%

  #  No patients have cancer
  mutate(SP_CNCR = FALSE) %>%

  # No patients have death dates
  mutate(BENE_DEATH_DT = NA_Date_)

# 20 patients are born before 1800
patients_01$BENE_BIRTH_DT[sample(1:nrow(patients_01), 20)] <- sample(17000101:17000131, size = 20)

# 100 random claim payment amounts are -10000
inpatient_01$CLM_PMT_AMT[sample(1:nrow(inpatient_01), 100)] <- sample(-(10000:30000), size = 100)

readr::write_csv(patients_01, file = "data/patients01.csv")
readr::write_csv(inpatient_01, file = "data/inpatient01.csv")
readr::write_csv(outpatient_01, file = "data/outpatient01.csv")
readr::write_csv(prescription_01, file = "data/prescription01.csv")

# Data 02 ======================================================================

# should do some manipulation ...

tables_02 <- tables_raw %>%
  purrr::map(.f = ~filter(.x, DESYNPUF_ID %in% patients_selected))

patients_02 <- tables_02$bene08
inpatient_02 <- tables_02$inpatient
outpatient_02 <- tables_02$outpatient
prescription_02 <- tables_02$prescription

# remove prescription info from after 2008
prescription_02 <- prescription_01 %>%
  filter(SRVC_DT <= 20090000)

patients_02 <- patients_02 %>%

  mutate(multiplier = case_when(
    BENE_RACE_CD == 2 ~ 40,
    TRUE ~ 50)) %>%

  mutate(daysafterdeath =  365 * multiplier + rnorm(10000, mean = 0, sd = 4000)) %>%

  mutate(BENE_DEATH_DT = lubridate::ymd(BENE_BIRTH_DT) + daysafterdeath) %>%
  mutate(BENE_DEATH_DT = case_when(
    BENE_DEATH_DT >= ymd("2008-12-31") ~ NA_Date_,
    TRUE ~ BENE_DEATH_DT
  )) %>%
  mutate(BENE_DEATH_DT = as.numeric(stringr::str_remove_all(as.character(BENE_DEATH_DT), "-"))) %>%
  mutate(BENE_DEATH_DT = case_when(
    BENE_BIRTH_DT >= BENE_DEATH_DT ~ BENE_DEATH_DT + 100,
    TRUE ~ BENE_DEATH_DT
  ))

patients_02$BENE_DEATH_DT[sample(10000, size = 2000)] <- NA

inpatient_02 <- inpatient_02 %>%
  filter(CLM_PMT_AMT > 0)

readr::write_csv(patients_02, file = "data/patients02.csv")
readr::write_csv(inpatient_02, file = "data/inpatient02.csv")
readr::write_csv(outpatient_02, file = "data/outpatient02.csv")
readr::write_csv(prescription_02, file = "data/prescription02.csv")


