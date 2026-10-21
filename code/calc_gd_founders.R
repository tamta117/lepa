# automate logging GD results
suppressPackageStartupMessages(library(tidyverse, quietly = T))

# define arguments
args <- commandArgs(trailingOnly = TRUE)
iter <- args[1]
mig_rate <- args[2]
sup <- args[3]
f <- args[4]

#### fam ----------------------------------------------------------------------------------------------------------------
fam <- read.table("slim/input_vcf/ch2/founders.fam")
gd <- read.table(paste0("slim/output/offspring/log/best", f, "_gd.txt"), header = T)%>%
  semi_join(fam, by = c("pair" = "V2"))%>%
  summarise(ho = mean(ho),
            roh = mean(roh))

#### pi -----------------------------------------------------------------------------------------------------------------
pi <- read.table("slim/output/ch2/gd/founders.windowed.pi", header = T) %>%
  janitor::clean_names() %>%
  summarise(pi = weighted.mean(pi, n_variants))

#### log file ------------------------------------------------------------------------------------------------------------
log <- bind_cols(pi, gd)%>%
  mutate(iter = iter,
         m = mig_rate,
         sup = sup,
         duration = 60,
         tick = 1,
         n = 4,
         f = f)%>%
  select(iter, f, m, duration, sup, tick, n, pi, ho, roh)
outfile <- "slim/output/ch2/log/gd.txt"
write.table(log, outfile, append = T, row.names = F, col.names = !file.exists(outfile), sep = "\t", quote = F)
