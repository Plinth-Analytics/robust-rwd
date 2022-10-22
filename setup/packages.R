#!/usr/bin/Rscript

if (!("renv" %in% rownames(installed.packages())) install.packages("renv")

renv::restore()

