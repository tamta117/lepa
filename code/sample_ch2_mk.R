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
  filter(scale_mk > mean(scale_mk))

# sample founders
if (f == "50") {
  if (mk == 0) {
    repeat {
      founders <- slice_sample(parents, n = 4)
      if (n_distinct(founders$wild) == 4 & n_distinct(founders$zoo) == 4) {
        founders <- select(founders, c(V1, V2))
        break
      }
    }
  } else {
    repeat {
      founders <- slice_sample(parents, n = 4, weight_by = parents$scale_mk)
      if (n_distinct(founders$wild) == 4 & n_distinct(founders$zoo) == 4) {
        founders <- select(founders, c(V1, V2))
        break
      }
    }
  }
} else {
  if (mk == 0) {
    repeat {
      founders <- slice_sample(parents, n = 4)
      if (n_distinct(founders$wild) == 4 & n_distinct(founders$f1) == 4 & n_distinct(founders$par2) == 4 & n_distinct(founders$par1) == 4 & any(founders$wild %in% founders$par1) == F) {
        founders <- select(founders, c(V1, V2))
        break
      }
    }
  } else {
    repeat {
      founders <- slice_sample(parents, n = 4, weight_by = parents$scale_mk)
      if (n_distinct(founders$wild) == 4 & n_distinct(founders$f1) == 4 & n_distinct(founders$par2) == 4 & n_distinct(founders$par1) == 4 & any(founders$wild %in% founders$par1) == F) {
        founders <- select(founders, c(V1, V2))
        break
      }
    }
  }
}
write.table(founders, "slim/files/ch2/founders.txt", row.names = F, col.names = F, quote = F, sep = "\t")
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --keep slim/files/ch2/founders.txt --silent --export vcf-4.2 --make-bed --out slim/input_vcf/ch2/founders"))

# sample supplement
left <- anti_join(parents, founders, by = "V2")
n_sup <- length(seq(1 + sup, 61, by = sup))
n_indv <- m * n_sup
if (mk == 0) {
  supplement <- sample_n(left, size = n_indv)%>%
    select(V1, V2)
} else {
  supplement <- sample_n(left, size = n_indv, weight = scale_mk)%>%
    select(V1, V2)
}
write.table(supplement, "slim/files/ch2/supplement.txt", row.names = F, col.names = F, quote = F, sep = "\t")
system(paste0("plink2 --bfile slim/input_vcf/offspring/all", f, " --keep slim/files/ch2/supplement.txt --silent --export vcf-4.2 --out slim/input_vcf/ch2/sup"))
