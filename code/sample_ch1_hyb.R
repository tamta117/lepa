# sample hybrid for ch1
suppressPackageStartupMessages(library(tidyverse, quietly = T))

# define arguments
args <- commandArgs(trailingOnly = TRUE)
mig_rate <- as.numeric(args[1])
freq <- as.numeric(args[2])
f <- args[3]

# sample
n <- mig_rate * (60 / freq)
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --thin-indiv-count ", n, " --export vcf-4.2 --out slim/input_vcf/ch1/", f))