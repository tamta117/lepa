# parent selection schemes
library(tidyverse)

# KING square -----------------------------------------------------------------------------------------------------------
# run KING
system("plink2 --bfile filter/maf/maf0.1_thin_wmzf --make-king square --out filter/maf/maf0.1_thin_wmzf")

# read in KING table
id <- read.table("filter/maf/maf0.1_thin_wmzf.king.id")
king <- read.table("filter/maf/maf0.1_thin_wmzf.king")%>%
  setNames(id$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)),
         iid = id$V2,
         sex = if_else(str_detect(iid, "^\\d"), "female", "male"))

# calculate mk
mk <- king%>%
  mutate(mk = rowMeans(pick(where(is.numeric)), na.rm = T))%>%
  select(iid, sex, mk)%>%
  
# calculate mean for each individual
pair <- mk%>%
  group_by(sex)%>%
  slice_min(mk, n = 1)
# individuals with highest FROH has lowest mean kinship

# let's try sorting by pop
wild <- king%>%
  filter(sex == "male")%>%
  select(!matches("^\\d"))%>%
  mutate(mk = rowMeans(pick(where(is.numeric)), na.rm = T))%>%
  select(iid, sex, mk)%>%
  arrange(mk)

# compare to original
wild_mk <- filter(mk, sex == "male")%>%
  arrange(mk)
compare_wild <- bind_cols(wild, wild_mk)

# KING table -----------------------------------------------------------------------------------------------------------
king <- read.table("filter/maf/maf0.1_thin_wmzf.kin0")%>%
  mutate(pair = paste0(V1, "_", V3))%>%
  filter(pair %in% c("brazilian_ranch", "brazilian_refuge", "ranch_brazilian", "ranch_generic", "refuge_brazilian", "refuge_generic"))
