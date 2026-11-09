# automate logging GD results
suppressPackageStartupMessages(library(tidyverse, quietly = T))


# define arguments
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
iter <- args[2]
mig_rate <- args[3]
duration <- args[4]
sup <- args[5]

# set constants
pop <- c("ranch", "refuge")
tick <- as.integer(str_extract_all(file, "\\d+"))

#### pi -----------------------------------------------------------------------------------------------------------------

for (p in pop) {
  df <- read.table(paste0("slim/output/ch1/gd/", file, "_", p, ".windowed.pi"), header = T) %>%
    janitor::clean_names() %>%
    summarise(pi = weighted.mean(pi, n_variants))%>%
    mutate(pop = p)
  assign(paste0("pi_", p), df)
}
pi <- bind_rows(pi_ranch, pi_refuge)

#### het ----------------------------------------------------------------------------------------------------------------

for (p in pop) {
  bim <- read.table(paste0("slim/output_vcf/ch1/", file, ".bim"))
  df <- read.table(paste0("slim/output/ch1/gd/", file, "_", p, ".het")) %>%
    mutate(ho = (V5 - V3) / nrow(bim),
           he = (V5 - V4) / nrow(bim))%>%
    summarise(he = mean(he),
              ho = mean(ho))%>%
    mutate(pop = p)
  assign(paste0("het_", p), df)
}
het <- bind_rows(het_ranch, het_refuge)

#### FROH ----------------------------------------------------------------------------------------------------------------

df <- read.table(paste0("slim/output/ch1/gd/", file, "_roh.txt")) 
colnames(df) <- c("type", "sample", "chromosome", "start", "end", "length_bp", "n_markers", "quality")
roh <- df%>%
  filter(quality >= 30 & length_bp >= 5000000)%>%
  group_by(sample)%>%
  summarise(bp_sum = sum(length_bp))%>%
  ungroup()%>%
  mutate(FROH = bp_sum / 2.47e9,
         pop = if_else(grepl("p1", sample), "ranch", "refuge"))%>%
  group_by(pop)%>%
  summarise(roh = median(FROH),
            n = n())

#### Fst ----------------------------------------------------------------------------------------------------------------

fst <- read.table(paste0("slim/output/ch1/gd/", file, ".windowed.weir.fst"), header = T)%>%
  janitor::clean_names()%>%
  summarise(fst = weighted.mean(weighted_fst, n_variants, na.rm = T))

#### log file ------------------------------------------------------------------------------------------------------------
log <- left_join(pi, het, by = "pop")%>%
  left_join(roh, by = "pop")%>%
  mutate(fst = fst$fst,
         iter = iter,
         m = mig_rate,
         duration = duration,
         sup = sup,
         tick = tick,
         migrant = "wild")%>%
  select(migrant, iter, m, duration, sup, tick, pop, n, pi, he, ho, roh, fst)
outfile <- "slim/output/ch1/log/gd_wild.txt"
write.table(log, outfile, append = T, row.names = F, col.names = !file.exists(outfile), sep = "\t", quote = F)
