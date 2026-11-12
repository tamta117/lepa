# check local allele freq
library(tidyverse)

# create cluster file
wild <- read.table("odata/wild.txt")
zoo <- read.table("odata/zoo.txt")
cluster <- bind_rows(wild, zoo %>% mutate(V2 = as.character(V2)), .id = "cluster")%>%
  mutate(cluster = if_else(cluster == "1", "wild", "zoo"))%>%
  relocate(cluster, .after = V2)%>%
  write.table("odata/family_cluster.txt", row.names = F, col.names = F, quote = F, sep = "\t")

# run af for wild and zoo
system("plink --bfile filter/maf/maf0.1_thin --freq --within odata/family_cluster.txt --out filter/maf/maf0.1_thin")

# read in af outputs
af <- read.table("filter/maf/maf0.1_thin.frq.strat", header = T)%>%
  janitor::clean_names()%>%
  select(1:6)%>%
  pivot_wider(names_from = clst, values_from = maf)
obshom <- filter(af, wild == 0 & zoo == 1)
# 7287 variants

# check with fst
system("plink --bfile filter/maf/maf0.1_thin_wmzf --fst --within odata/family_cluster.txt --out filter/maf/maf0.1_thin_wmzf")
fst <- read.table("filter/maf/maf0.1_thin_wmzf.fst", header = T)%>%
  janitor::clean_names()%>%
  filter(fst == 1)
# 7915 variants

# find which ones are in fst but not in af
new <- anti_join(fst, obshom, by = c("chr", "snp"))
# basically where af = 0.01 -- was not accounted for earlier because we filtered for 0

# find shared loci between fst and af
same <- semi_join(af, fst, by = c("chr", "snp"))
ggplot(same, aes(wild, zoo))+
  geom_point()

# check that REF/ALT matches bim
bim <- read.table("filter/maf/maf0.1_thin.bim")%>%
  semi_join(same, by = c("V1" = "chr", "V2" = "snp"))%>%
  mutate(REF = same$a2,
         ALT = same$a1)
length(which(bim$V6 == bim$REF)) # 7915
# final outliers = 7915 where REF is predominant in wild (0-0.08) and not found in zoo

# export outliers
write.table(select(bim, 1:2), "filter/maf/maf0.1_thin_outliers.txt", row.names = F, col.names = F, quote = F, sep = "\t")
