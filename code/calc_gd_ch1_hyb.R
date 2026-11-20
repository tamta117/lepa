# automate logging GD
suppressPackageStartupMessages(library(tidyverse, quietly = T))

# define arguments
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
iter <- args[2]
mig_rate <- args[3]
sup <- args[4]
f <- args[5]
pop <- args[6]

# set constants
tick <- as.integer(str_extract_all(file, "\\d+"))

#### local af -----------------------------------------------------------------------------------------------------------
system(paste0("plink --bfile slim/output_vcf/ch1/", file, " --freq --family --out slim/output/ch1/gd/", file))
outliers <- read.table("filter/maf/maf0.1_thin_outliers.txt", header = T)
af <- read.table(paste0("slim/output/ch1/gd/", file, ".frq.strat"), header = T)%>%
  janitor::clean_names()%>%
  semi_join(outliers, by = c("chr", "snp"))%>%
  left_join(outliers, by = c("chr", "snp"))%>%
  mutate(af = if_else(REF == a2, 1 - maf, maf))%>%
  summarise(laf = mean(af))

#### het ----------------------------------------------------------------------------------------------------------------
bim <- read.table(paste0("slim/output_vcf/ch1/", file, ".bim"))
het <- read.table(paste0("slim/output/ch1/gd/", file, ".het")) %>%
  mutate(ho = (V5 - V3) / nrow(bim))%>%
  summarise(ho = mean(ho),
            n = n())

#### FROH ----------------------------------------------------------------------------------------------------------------
roh <- tryCatch(read.table(paste0("slim/output/ch1/gd/", file, "_roh.txt")), error = function(e) {
  if (grepl("no lines available in input", conditionMessage(e), fixed = T)) { data.frame(roh = NA) }})
if (ncol(roh) == 1) {
  roh <- df
} else {
  colnames(roh) <- c("type", "sample", "chromosome", "start", "end", "length_bp", "n_markers", "quality")
  roh <- roh%>%
    filter(quality >= 30 & length_bp >= 5000000)%>%
    group_by(sample)%>%
    summarise(bp_sum = sum(length_bp))%>%
    mutate(FROH = bp_sum / 2.47e9)%>%
    summarise(roh = mean(FROH))
}

#### log file ------------------------------------------------------------------------------------------------------------
log <- bind_cols(roh, het, af)%>%
  mutate(pop = pop,
         fst = NA,
         iter = iter,
         m = mig_rate,
         migrant = f,
         sup = sup,
         tick = tick,
         n = if_else(is.na(n), 0, n))%>%
  select(migrant, iter, m, sup, tick, pop, n, ho, roh, fst, laf)
outfile <- "slim/output/ch1/log/gd.txt"
write.table(log, outfile, append = T, row.names = F, col.names = !file.exists(outfile), sep = "\t", quote = F)
