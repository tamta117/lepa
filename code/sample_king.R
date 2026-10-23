# parent selection schemes

# run KING
system("plink2 --bfile filter/maf/maf0.1_thin_wmzf --make-king-table --out filter/maf/maf0.1_thin_wmzf")

# read in KING table
king <- read.table("filter/maf/maf0.1_thin_wmzf.kin0")
names(king) <- c("fid1", "iid1", "fid2", "iid2", "n_loci", "hethet", "obshom", "k")

# make matrix
df <- data.frame(iid1 = king$iid2,
                 iid2 = king$iid1,
                 k = king$k)
matrix <- bind_rows(select(king, c(iid1, iid2, k)), df)%>%
  mutate(across(starts_with("iid"), ~ str_remove(., "_24|_25|_22")))%>%
  pivot_wider(names_from = iid2, values_from = k)
  

# calculate mean for each individual
mk <- matrix%>%
  mutate(mk = rowMeans(pick(contains("_")), na.rm = T))%>%
  select(indv1, mk)%>%
  mutate(sex = if_else(str_detect(indv1, "ranch|refuge"), "male", "female"))
pair <- mk%>%
  arrange(mk)%>%
  group_by(sex)%>%
  summarise(mk = min(mk, na.rm = T),
            across(indv1, first))



