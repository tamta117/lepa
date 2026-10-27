# parent selection schemes
library(tidyverse)

# 50% -----------------------------------------------------------------------------------------------------------
# run KING
system("plink2 --bfile filter/maf/maf0.1_thin_wmzf --make-king square --out filter/maf/maf0.1_thin_wmzf")

# read in KING table
id <- read.table("filter/maf/maf0.1_thin_wmzf.king.id")%>%
  separate(V2, into = c("V2", "remove"), sep = "_")
king <- read.table("filter/maf/maf0.1_thin_wmzf.king")%>%
  setNames(id$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)))

# calculate mk
mk <- king%>%
  mutate(mk = abs(rowMeans(as.matrix(king), na.rm = T)),
         iid = id$V2,
         sex = if_else(str_detect(iid, "^\\d"), "female", "male"))%>%
  select(iid, sex, mk)
write.table(mk, "filter/maf/maf0.1_thin_wmzf_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# sample by weight and sex
n <- 4
pairs <- mk%>%
  group_by(sex)%>%
  sample_n(size = n, weight = mk)%>%
  arrange(mk)

# make pair names
pairnames <- bind_cols(subset(pairs, sex == "female"), subset(pairs, sex == "male"))%>%
  mutate(pair = paste0(iid...4, "_", iid...1),
         pop = "p1")%>%
  select(pop, pair)
write.table(pairnames, "slim/files/offspring/founders_mk.txt", row.names = F, col.names = F, quote = F, sep = "\t")

# subset pair
system("plink2 --bfile slim/input_vcf/offspring/all50 --keep slim/files/offspring/founders_mk.txt --export vcf-4.2 --make-bed --out slim/input_vcf/ch2/founders")

# write mk for all pairs
parents <- expand.grid(mk$iid[mk$sex == "male"], mk$iid[mk$sex == "female"])%>%
  left_join(select(mk, c(iid, mk1 = mk)), by = c("Var1" = "iid"))%>%
  left_join(select(mk, c(iid, mk2 = mk)), by = c("Var2" = "iid"))%>%
  mutate(sum_mk = mk1 + mk2,
         pairs = paste0(Var1, "_", Var2),
         pop = "p1")
write.table(parents, "slim/files/50_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# 75% ------------------------------------------------------------------------------------------------------------------
# run KING
system("plink2 --bfile slim/input_vcf/offspring/wm_all50 --make-king square --out slim/input_vcf/offspring/wm_all50")

# read in KING table
id <- read.table("slim/input_vcf/offspring/wm_all50.king.id")
king <- read.table("slim/input_vcf/offspring/wm_all50.king")%>%
  setNames(id$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)))

# calculate mk
mk <- king%>%
  mutate(mk = abs(rowMeans(as.matrix(king), na.rm = T)),
         iid = id$V2,
         sex = if_else(str_detect(iid, "_"), "female", "male"))%>%
  select(iid, sex, mk)
write.table(mk, "slim/input_vcf/offspring/75_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# sample by weight and sex
n <- 4
pairs <- mk%>%
  group_by(sex)%>%
  sample_n(size = n, weight = mk)%>%
  arrange(mk)

# make pair names
pairnames <- bind_cols(subset(pairs, sex == "female"), subset(pairs, sex == "male"))%>%
  mutate(pair = paste0(iid...4, "_", iid...1),
         pop = "p1")%>%
  select(pop, pair)
write.table(pairnames, "slim/files/offspring/founders_mk.txt", row.names = F, col.names = F, quote = F, sep = "\t")

# subset pair
system("plink2 --bfile slim/input_vcf/offspring/all50 --keep slim/files/offspring/founders_mk.txt --export vcf-4.2 --make-bed --out slim/input_vcf/ch2/founders")

# write mk for all pairs
parents <- expand.grid(mk$iid[mk$sex == "male"], mk$iid[mk$sex == "female"])%>%
  left_join(select(mk, c(iid, mk1 = mk)), by = c("Var1" = "iid"))%>%
  left_join(select(mk, c(iid, mk2 = mk)), by = c("Var2" = "iid"))%>%
  mutate(sum_mk = mk1 + mk2,
         pairs = paste0(Var1, "_", Var2),
         pop = "p1")
write.table(parents, "slim/files/50_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")