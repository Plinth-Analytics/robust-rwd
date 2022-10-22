#!/usr/bin/Rscript

if (!("renv" %in% rownames(installed.packages("renv"))) install.packages("renv")

renv::restore()

