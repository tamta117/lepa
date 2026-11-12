# automate logging GD results
suppressPackageStartupMessages(library(tidyverse, quietly = T))


# define arguments
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
iter <- args[2]
mig_rate <- args[3]
duration <- args[4]
sup <- args[5]
maxage <- args[6]

# set constants
pop <- c("ranch", "refuge")
tick <- as.integer(str_extract_all(file, "\\d+"))

#### local af -----------------------------------------------------------------------------------------------------------
system(paste0("plink --bfile slim/output_vcf/ch1/", file, " --freq --family --out slim/output/ch1/gd/", file))
outliers <- read.table("filter/maf/maf0.1_thin_outliers.txt", header = T)
af <- read.table(paste0("slim/output/ch1/gd/", file, ".frq.strat"), header = T)%>%
  janitor::clean_names()%>%
  semi_join(outliers, by = c("chr", "snp"))%>%
  left_join(outliers, by = c("chr", "snp"))%>%
  mutate(af = if_else(REF == a2, 1 - maf, maf),
         pop = if_else(clst == "p1", "ranch", "refuge"))%>%
  group_by(pop)%>%
  summarise(laf = mean(af))

#### het ----------------------------------------------------------------------------------------------------------------

bim <- read.table(paste0("slim/output_vcf/ch1/", file, ".bim"))
het <- read.table(paste0("slim/output/ch1/gd/", file, ".het")) %>%
  mutate(ho = (V5 - V3) / nrow(bim),
         he = (V5 - V4) / nrow(bim),
         pop = if_else(V1 == "p1", "ranch", "refuge"))%>%
  group_by(pop)%>%
  summarise(he = mean(he),
            ho = mean(ho),
            n = n())

#### FROH ----------------------------------------------------------------------------------------------------------------

df <- tryCatch(read.table(paste0("slim/output/ch1/gd/", file, "_roh.txt")), error = function(e) {
  if (grepl("no lines available in input", conditionMessage(e), fixed = T)) { data.frame(roh = NA,
                                                                                         pop = c("ranch", "refuge")) }})
if (ncol(df) == 2) {
  roh <- df
} else {
  colnames(df) <- c("type", "sample", "chromosome", "start", "end", "length_bp", "n_markers", "quality")
  roh <- df%>%
    filter(quality >= 30 & length_bp >= 5000000)%>%
    group_by(sample)%>%
    summarise(bp_sum = sum(length_bp))%>%
    mutate(FROH = bp_sum / 2.47e9,
           pop = if_else(grepl("p1", sample), "ranch", "refuge"))%>%
    group_by(pop)%>%
    summarise(roh = mean(FROH))
}

#### Fst ----------------------------------------------------------------------------------------------------------------

if (file.exists(paste0("slim/output/ch1/gd/", file, ".windowed.weir.fst"))) {
  fst <- read.table(paste0("slim/output/ch1/gd/", file, ".windowed.weir.fst"), header = T)%>%
    janitor::clean_names()%>%
    summarise(fst = weighted.mean(weighted_fst, n_variants, na.rm = T))
} else {
  fst <- data.frame(fst = NA)
}

#### log file ------------------------------------------------------------------------------------------------------------

log <- left_join(roh, het, by = "pop")%>%
  left_join(af, by = "pop")%>%
  mutate(fst = fst$fst,
         iter = iter,
         m = mig_rate,
         duration = duration,
         sup = sup,
         tick = tick,
         migrant = if_else(mig_rate == 0, "none", "wild"),
         max_repro_age = maxage,
         n = if_else(is.na(n), 0, n))%>%
  select(migrant, iter, m, duration, sup, max_repro_age, tick, pop, n, he, ho, roh, fst, laf)
outfile <- "slim/output/ch1/log/rerun_maxage13.txt"
write.table(log, outfile, append = T, row.names = F, col.names = !file.exists(outfile), sep = "\t", quote = F)
