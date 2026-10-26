# automate logging GD
suppressPackageStartupMessages(library(tidyverse, quietly = T))

# define arguments
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
iter <- args[2]
mig_rate <- args[3]
sup <- args[4]
duration <- args[5]
f <- args[6]
mk <- args[7]

# set constants
tick <- str_extract(file, "\\d+")

#### pi -----------------------------------------------------------------------------------------------------------------
pi <- read.table(paste0("slim/output/ch2/gd/", file, ".windowed.pi"), header = T) %>%
  janitor::clean_names() %>%
  summarise(pi = weighted.mean(pi, n_variants))

#### het ----------------------------------------------------------------------------------------------------------------
het <- read.table(paste0("slim/output/ch2/gd/", file, ".het")) %>%
  mutate(ho = (V5 - V3) / 430782)%>%
  summarise(ho = mean(ho),
            n = n())

#### FROH ----------------------------------------------------------------------------------------------------------------
roh <- tryCatch(read.table(paste0("slim/output/ch2/gd/", file, "_roh.txt")), error = function(e) {
  if (grepl("no lines available in input", conditionMessage(e), fixed = T)) { data.frame(roh = NA) }})
if (ncol(roh) == 1) {
  names(roh) <- "roh"
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
log <- bind_cols(pi, het)%>%
  bind_cols(roh)%>%
  mutate(iter = iter,
         duration = duration,
         sup = sup,
         m = mig_rate,
         tick = tick,
         f = f,
         mk = mk)%>%
  select(iter, f, mk, m, duration, sup, tick, n, pi, ho, roh)
outfile <- "slim/output/ch2/log/gd.txt"
write.table(log, outfile, append = T, row.names = F, col.names = !file.exists(outfile), sep = "\t", quote = F)
