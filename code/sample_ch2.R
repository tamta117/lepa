# sample founders and supplement for ch2
suppressPackageStartupMessages(library(tidyverse, quietly = T))

# define arguments
args <- commandArgs(trailingOnly = TRUE)
m <- as.numeric(args[1])
sup <- as.numeric(args[2])
f <- args[3]

# sample founders
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --thin-indiv-count 4 --silent --export vcf-4.2 --out slim/input_vcf/ch2/founders"))

# sample supplement
n_sup <- length(seq(1 + sup, 61, by = sup))
n_indv <- m * n_sup
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --thin-indiv-count ", n_indv, " --silent --export vcf-4.2 --out slim/input_vcf/ch2/sup", n_indv))