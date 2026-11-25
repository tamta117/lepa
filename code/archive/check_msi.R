# check msi

# read in pmx output
msi <- read.table("pmx/msi_table.txt", header = T, sep = "\t")%>%
  janitor::clean_names()%>%
  arrange(ext_msi)

# confirm with best offspring
offspring <- read.table("slim/output/offspring/log/best50_gd.txt", header = T)%>%
  arrange(roh, desc(ho))
