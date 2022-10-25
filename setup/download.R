#!/usr/bin/Rscript

library(purrr)

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
    "inpatient" = "DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.zip"
  ),
  ~ download.file(url = downloads_url(.x), destfile = destination(.y))
)
