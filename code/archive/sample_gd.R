# find individual with highest diversity and lowest inbreeding

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
