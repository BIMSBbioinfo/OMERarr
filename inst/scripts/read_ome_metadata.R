#!/usr/bin/env Rscript

# libraries
library(OMERarr)
args <- commandArgs(trailingOnly = TRUE)
ome_file <- args[1]
cat('{"valid":true}', "\n")