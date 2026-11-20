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

# write mk for all pairs
fam <- read.table("slim/input_vcf/offspring/all75.fam")
parents <- expand.grid(mk$iid[mk$sex == "male"], mk$iid[mk$sex == "female"])%>%
  left_join(select(mk, c(iid, mk1 = mk)), by = c("Var1" = "iid"))%>%
  left_join(select(mk, c(iid, mk2 = mk)), by = c("Var2" = "iid"))%>%
  separate(Var2, into = c("par1", "par2"), sep = "_", remove = F)%>%
  filter(Var1 != par1)%>%
  mutate(sum_mk = mk1 + mk2,
         pairs = paste0(Var1, "_", Var2),
         pop = "p1")%>%
  semi_join(fam, by = c("pairs" = "V2"))
write.table(parents, "slim/files/75_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# compare ROH vs KING ---------------------------------------------------------------------------------------------------
roh <- read.table("filter/maf/maf0.1_thin_roh.txt")%>%
  filter(V8 > 30 & V6 > 5000000)%>%
  group_by(V2)%>%
  summarise(sum = sum(V6))%>%
  mutate(froh = sum / 2.47e9)%>%
  separate(V2, into = c("pop", "indv"))
mk <- read.table("filter/maf/maf0.1_thin_wmzf_mk.txt", header = T)

# plot
roh_mk <- left_join(mk, roh, by = c("iid" = "indv"))
ggplot(roh_mk, aes(froh, mk))+
  geom_jitter()+
  ggrepel::geom_text_repel(aes(label = iid))

# sample GD instead ---------------------------------------------------------------------------------------------------
#### 50% --------------------------------------------------------------------------------------------------------------
# read in data
roh <- read.table("filter/maf/maf0.1_thin_roh.txt")%>%
  filter(V8 > 30 & V6 > 100000)%>%
  group_by(V2)%>%
  summarise(sum = sum(V6))%>%
  mutate(froh = sum / 2.47e9)%>%
  separate(V2, into = c("pop", "indv"))
ho <- read.table("filter/maf/maf0.1_thin.het", header = T)%>%
  janitor::clean_names()%>%
  mutate(ho = (n_nm - o_hom) / n_nm)%>%
  separate(iid, into = c("indv", "rem"))

# merge data
roh_ho <- left_join(roh, ho, by = "indv")%>%
  select(pop, indv, froh, ho)

# plot
ggplot(roh_ho, aes(froh, ho, color = pop))+
  geom_jitter()

# find highest ho and lowest froh
best <- roh_ho%>%
  mutate(ho_scale = rescale(ho),
         froh_scale = rescale(froh, to = c(1, 0)),
         sum = ho_scale + froh_scale)%>%
  arrange(sum)

# form pairs
parents <- read.table("slim/input_vcf/offspring/all50.fam")%>%
  separate(V2, into = c("wild", "zoo"), remove = F)%>%
  left_join(select(best, c(indv, wild_gd = sum)), by = c("wild" = "indv"))%>%
  left_join(select(best, c(indv, zoo_gd = sum)), by = c("zoo" = "indv"))%>%
  mutate(sum_gd = wild_gd + zoo_gd)
write.table(parents, "slim/files/50_gd.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# sample
sample <- slice_sample(parents, n = 4, weight_by = parents$sum_gd)

#### 75% --------------------------------------------------------------------------------------------------------------
gd <- read.table("slim/output/offspring/log/best75_gd.txt", header = T)%>%
  mutate(across(ends_with("_ho"), ~ rescale(.)),
         across(ends_with("_roh"), ~ rescale(., to = c(1, 0))),
         sum_gd = par1_ho + par2_ho + par1_roh + par2_roh,
         pop = "p1")%>%
  separate(pair, into = c("wild", "par1", "par2"), remove = F)%>%
  select(pop, 1:4, sum_gd)
write.table(gd, "slim/files/75_gd.txt", row.names = F, col.names = T, quote = F, sep = "\t")

gd <- read.table("slim/output/offspring/log/best50_gd.txt", header = T)%>%
  mutate(across(ends_with("_ho"), ~ rescale(.)),
         across(ends_with("_roh"), ~ rescale(., to = c(1, 0))),
         sum_gd = par1_ho + par2_ho + par1_roh + par2_roh,
         pop = "p1")%>%
  separate(pair, into = c("wild", "zoo"), remove = F)%>%
  select(pop, 1:3, sum_gd)
write.table(gd, "slim/files/50_gd.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# try population-specific -----------------------------------------------------------------------------------------------------------

#### 50% --------------------------------------------------------------------------------------------------------------
# run KING
system("plink2 --bfile filter/maf/maf0.1_thin_wild --make-king square --out filter/maf/maf0.1_thin_wild")
system("plink2 --bfile filter/maf/maf0.1_thin_zoo --make-king square --out filter/maf/maf0.1_thin_zoo")

# read in KING
id_wild <- read.table("filter/maf/maf0.1_thin_wild.king.id")
king_wild <- read.table("filter/maf/maf0.1_thin_wild.king")%>%
  setNames(id_wild$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)))
id_zoo <- read.table("filter/maf/maf0.1_thin_zoo.king.id")
king_zoo <- read.table("filter/maf/maf0.1_thin_zoo.king")%>%
  setNames(id_zoo$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)))

# calculate mk
mk_wild <- king_wild%>%
  mutate(mk = abs(rowMeans(as.matrix(king_wild), na.rm = T)),
         iid = id_wild$V2)%>%
  select(iid, mk)
mk_zoo <- king_zoo%>%
  mutate(mk = abs(rowMeans(as.matrix(king_zoo), na.rm = T)),
         iid = as.character(id_zoo$V2))%>%
  select(iid, mk)

# merge
wmzf <- read.table("odata/wmzf.txt")
mk <- bind_rows(mk_wild, mk_zoo)%>%
  semi_join(wmzf, by = c("iid" = "V2"))%>%
  separate(iid, into = c("iid", "rem"))

# join with 50% fam
fam <- read.table("slim/input_vcf/offspring/all50.fam")%>%
  separate(V2, into = c("wild", "zoo"), remove = F)%>%
  left_join(select(mk, c(iid, wild_mk = mk)), by = c("wild" = "iid"))%>%
  left_join(select(mk, c(iid, zoo_mk = mk)), by = c("zoo" = "iid"))%>%
  mutate(sum_mk = wild_mk + zoo_mk,
         scale_mk = rescale(sum_mk, to = c(1, 0)))%>%
  select(-c(5:8))
write.table(fam, "slim/files/50_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

#### 75% --------------------------------------------------------------------------------------------------------------
# run KING
system("plink2 --bfile slim/input_vcf/offspring/all50 --make-king square --out slim/input_vcf/offspring/all50")

# read in KING
id_wild <- read.table("filter/maf/maf0.1_thin_wild.king.id")
king_wild <- read.table("filter/maf/maf0.1_thin_wild.king")%>%
  setNames(id_wild$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)))
id_f1 <- read.table("slim/input_vcf/offspring/all50.king.id")
king_f1 <- read.table("slim/input_vcf/offspring/all50.king")%>%
  setNames(id_f1$V2)%>%
  mutate(across(everything(), ~ if_else(. == 0.5, NA, .)))

# calculate mk
mk_wild <- king_wild%>%
  mutate(mk = abs(rowMeans(as.matrix(king_wild), na.rm = T)),
         iid = id_wild$V2)%>%
  separate(iid, into = c("iid", "rem"))%>%
  select(iid, mk)
mk_f1 <- king_f1%>%
  mutate(mk = abs(rowMeans(as.matrix(king_f1), na.rm = T)),
         iid = as.character(id_f1$V2))%>%
  select(iid, mk)

# merge
mk <- bind_rows(mk_wild, mk_f1)

# join with 75% fam
fam <- read.table("slim/input_vcf/offspring/all75.fam")%>%
  separate(V2, into = c("wild", "par1", "par2"), remove = F)%>%
  mutate(f1 = paste0(par1, "_", par2))%>%
  left_join(select(mk, c(iid, wild_mk = mk)), by = c("wild" = "iid"))%>%
  left_join(select(mk, c(iid, f1_mk = mk)), by = c("f1" = "iid"))%>%
  mutate(sum_mk = wild_mk + f1_mk,
         scale_mk = rescale(sum_mk, to = c(1, 0)))%>%
  select(-c(6:9))
write.table(fam, "slim/files/75_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

# plot offspring gd with parents' mk ---------------------------------------------------------------------------------------
# read in data
gd50 <- read.table("slim/output/offspring/log/best50_gd.txt", header = T)
mk50 <- read.table("slim/files/50_mk.txt", header = T)%>%
  left_join(select(gd50, c(pair, ho, roh)), by = c("V2" = "pair"))
gd75 <- read.table("slim/output/offspring/log/best75_gd.txt", header = T)
mk75 <- read.table("slim/files/75_mk.txt", header = T)%>%
  left_join(select(gd75, c(pair, ho, roh)), by = c("V2" = "pair"))

# plot
ggplot(mk50, aes(roh, scale_mk))+
  geom_jitter()
ggplot(mk75, aes(roh, scale_mk))+
  geom_jitter()
ggplot(mk50, aes(ho, scale_mk))+
  geom_jitter()
ggplot(mk75, aes(ho, scale_mk))+
  geom_jitter()
# conclusion: no pattern between sum_mk and ho or roh
# let's try just sire mk

# let's try sire mk
ggplot(mk50, aes(roh, wild_mk))+
  geom_jitter()
ggplot(mk75, aes(roh, wild_mk))+
  geom_jitter()

# plot parent roh and offspring roh
ggplot(gd50, aes(par1_roh, roh))+
  geom_jitter()+
  labs(x = "Sire FROH",
       y = "F1 FROH")
ggplot(gd75, aes(par1_roh, roh))+
  geom_jitter()+
  labs(x = "Sire FROH",
       y = "F2 FROH")

# difference between par1 and par2 roh
gd50$par_roh_diff <- gd50$par1_roh - gd50$par2_roh
ggplot(gd50, aes(par_roh_diff, roh))+
  geom_jitter()
gd75$par_roh_diff <- gd75$par1_roh - gd75$par2_roh
ggplot(gd75, aes(par_roh_diff, roh))+
  geom_jitter()

# conclusion: no pattern between parent mk and roh with offspring gd, likely need to consider where the roh are instead of how many there are

# use pairwise KING values ------------------------------------------------------------------------------------------------------
#### 50% ####
# run king
system("plink2 --bfile filter/maf/maf0.1_thin_wmzf --make-king-table --out filter/maf/maf0.1_thin_wmzf")

# read in KING table
king <- read.table("filter/maf/maf0.1_thin_wmzf.kin0")
colnames(king) <- c("fid1", "iid1", "fid2", "iid2", "nsnp", "hethet", "obshom", "k")
king <- king%>%
  mutate(across(starts_with("iid"), ~ str_remove_all(., "_24|_25|_22")),
         group = paste0(fid1, "_", fid2))%>%
  filter(!(group %in% c("brazilian_brazilian", "brazilian_generic", "generic_brazilian", "generic_generic", "ranch_ranch", "refuge_refuge")))%>%
  mutate(pair = if_else(fid1 %in% c("brazilian", "generic") & fid2 %in% c("ranch", "refuge"), paste0(iid2, "_", iid1), paste0(iid1, "_", iid2)))

# merge with gd
gd <- read.table("slim/output/offspring/log/best50_gd.txt", header = T)%>%
  left_join(select(king, c(pair, hethet, obshom, k)), by = "pair")

# plot pattern of offspring
ggplot(gd, aes(obshom, roh))+
  geom_jitter()+
  labs(x = "Proportion of opposite homozygous variants in parents",
       y = expression(F[ROH] ~ of ~ offspring),
       title = "50% Texas ancestry")
ggplot(gd, aes(obshom, ho))+
  geom_jitter()
ggplot(gd, aes(k, roh))+
  geom_jitter()+
  labs(x = "Pairwise kinship between parents",
       y = expression(F[ROH] ~ of ~ offspring),
       title = "50% Texas ancestry")
ggplot(gd, aes(k, ho))+
  geom_jitter()
ggplot(gd, aes(hethet, roh))+
  geom_jitter()
ggplot(gd, aes(hethet, ho))+
  geom_jitter()
# no pattern whatsoever, strongest pattern probably obshom x roh but still weak
# probably weak because range of roh is 0-0.006
# let's try it with 75%

# write for sim
mk <- read.table("slim/files/50_mk.txt", header = T)%>%
  left_join(select(king, c(pair, hethet, obshom, k)), by = c("V2" = "pair"))
write.table(mk, "slim/files/50_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")

#### 75% ----------------------------------------------------------------------------------------------------------------
# run king
system("plink2 --bfile slim/input_vcf/offspring/wm_all50 --make-king-table --out slim/input_vcf/offspring/wm_all50")

# read in KING table
king <- read.table("slim/input_vcf/offspring/wm_all50.kin0")
colnames(king) <- c("fid1", "iid1", "fid2", "iid2", "nsnp", "hethet", "obshom", "k")
king <- king%>%
  filter(!(fid1 == "p1" & fid2 == "p1"))%>%
  mutate(across(starts_with("iid"), ~ str_remove_all(., "_24|_25|_22")),
         group = paste0(fid1, "_", fid2))%>%
  filter(group == "ranch_p1" | group == "refuge_p1")%>%
  mutate(pair = paste0(iid1, "_", iid2))

# merge with gd
gd <- read.table("slim/output/offspring/log/best75_gd.txt", header = T)%>%
  left_join(select(king, c(pair, hethet, obshom, k)), by = "pair")

# plot pattern of offspring
ggplot(gd, aes(obshom, roh))+
  geom_jitter()+
  labs(x = "Proportion of opposite homozygous variants in parents",
       y = expression(F[ROH] ~ of ~ offspring),
       title = "75% Texas ancestry")
ggplot(gd, aes(obshom, ho))+
  geom_jitter()
ggplot(gd, aes(k, roh))+
  geom_jitter()+
  labs(x = "Pairwise kinship between parents",
       y = expression(F[ROH] ~ of ~ offspring),
       title = "75% Texas ancestry")
ggplot(gd, aes(k, ho))+
  geom_jitter()
ggplot(gd, aes(hethet, roh))+
  geom_jitter()
ggplot(gd, aes(hethet, ho))+
  geom_jitter()
# clear, strong relationship with higher obshom and lower roh

# write for sim
mk <- read.table("slim/files/75_mk.txt", header = T)%>%
  left_join(select(king, c(pair, hethet, obshom, k)), by = c("V2" = "pair"))
write.table(mk, "slim/files/75_mk.txt", row.names = F, col.names = T, quote = F, sep = "\t")
