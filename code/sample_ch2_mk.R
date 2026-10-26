# sample founders and supplement for ch2
suppressPackageStartupMessages(library(tidyverse, quietly = T))

# define arguments
args <- commandArgs(trailingOnly = TRUE)
m <- as.numeric(args[1])
sup <- as.numeric(args[2])
f <- args[3]
mk <- as.numeric(args[4])

# read in mk
parents <- read.table(paste0("slim/files/", f, "_mk.txt"), header = T)%>%
  filter(sum_mk > mean(sum_mk))

# sample founders
if (mk == 0) {
  repeat {
    founders <- slice_sample(parents, n = 4)
    if (n_distinct(founders$Var1) == 4 & n_distinct(founders$Var2) == 4) {
      founders <- select(founders, c(pop, pairs))
      break
    }
  }
} else {
  repeat {
    founders <- slice_sample(parents, n = 4, weight_by = parents$sum_mk)
    if (n_distinct(founders$Var1) == 4 & n_distinct(founders$Var2) == 4) {
      founders <- select(founders, c(pop, pairs))
      break
    }
  }
}
write.table(founders, "slim/files/ch2/founders.txt", row.names = F, col.names = F, quote = F, sep = "\t")
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --keep slim/files/ch2/founders.txt --silent --export vcf-4.2 --make-bed --out slim/input_vcf/ch2/founders"))

# sample supplement
left <- anti_join(parents, founders, by = "pairs")
n_sup <- length(seq(1 + sup, 61, by = sup))
n_indv <- m * n_sup
if (mk == 0) {
  supplement <- sample_n(left, size = n_indv)%>%
    select(pop, pairs)
} else {
  supplement <- sample_n(left, size = n_indv, weight = sum_mk)%>%
    select(pop, pairs)
}
write.table(supplement, "slim/files/ch2/supplement.txt", row.names = F, col.names = F, quote = F, sep = "\t")
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --keep slim/files/ch2/supplement.txt --silent --export vcf-4.2 --out slim/input_vcf/ch2/sup"))
