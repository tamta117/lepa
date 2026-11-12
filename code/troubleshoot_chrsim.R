# subset chromosome
for (i in 1:16) {
  system(paste0("plink2 --vcf filter/maf/maf0.1_thin_ranch.vcf --chr ", i, " --export vcf-4.2 --out filter/maf/maf0.1_thin_ranch_ch", i))
}
bcftools query -f '%CHROM\t%POS\t%REF\t%ALT[\t%GT]\n' "filter/maf/maf0.1_thin_ranch_ch1.vcf" > "filter/maf/maf0.1_thin_ranch_ch1_gt.txt"
bcftools query -l "filter/maf/maf0.1_thin_ranch_ch1.vcf" > "filter/maf/maf0.1_thin_ranch_ch1_samples.txt"

# check that same rules apply to post-processing
# do 0/0 get filtered out by slim?
pre_gt <- read.table("filter/maf/maf0.1_thin_ranch_ch1_gt.txt")%>%
  mutate(n0 = rowSums(across(-c(1:4)) == "0/0"),
         n1 = rowSums(across(-c(1:4)) == "0/1" | across(-c(1:4)) == "1/0"),
         n2 = rowSums(across(-c(1:4)) == "1/1"))%>%
  select(1:4, 30:32)
# some fixed 0/0 but no fixed 1/1

# get gt file for output vcf
bcftools query -f '%CHROM\t%POS\t%REF\t%ALT[\t%GT]\n' "slim/output_vcf/ch1/chr/1.vcf" > "slim/output_vcf/ch1/chr/1_gt.txt"
bcftools query -l "slim/output_vcf/ch1/chr/1.vcf" > "slim/output_vcf/ch1/chr/1_samples.txt"
post_gt <- read.table("slim/output_vcf/ch1/chr/1_gt.txt")%>%
  mutate(n0 = rowSums(across(-c(1:4)) == "0|0"),
         n1 = rowSums(across(-c(1:4)) == "0|1" | across(-c(1:4)) == "1|0"),
         n2 = rowSums(across(-c(1:4)) == "1|1"))%>%
  select(1:4, 125:127)
# no fixed 0/0 and 1/1

# find the lost loci
lost <- anti_join(pre_gt, post_gt, by = "V2")
hist(lost$n0) # basically all 0/0, but not all
same <- semi_join(pre_gt, post_gt, by = "V2")
hist(same$n0) # very diverse

# find new loci
new <- anti_join(post_gt, pre_gt, by = "V2") # 511 new, all only in 1 individual

# join pre and post
join <- inner_join(pre_gt, post_gt, by = "V2")%>%
  janitor::clean_names()
# nrow(subset(join, V4.x == V4.y))
# all REF/ALT matches

# plot
ggplot(join, aes(n0_x, n0_y))+
  geom_smooth()
ggplot(join, aes(n2_x, n2_y))+
  geom_point()

# compare het
system("plink2 --vcf filter/maf/maf0.1_thin_ranch_ch1.vcf --freq --out filter/maf/maf0.1_thin_ranch_ch1")
system("plink2 --vcf filter/maf/maf0.1_thin_ranch_ch1.vcf --het --read-freq filter/maf/maf0.1_thin_ranch_ch1.afreq --out filter/maf/maf0.1_thin_ranch_ch1")
system("plink2 --vcf slim/output_vcf/ch1/chr/1.vcf --het --out slim/output_vcf/ch1/chr/1")
pre_het <- read.table("filter/maf/maf0.1_thin_ranch_ch1.het")%>%
  mutate(ho = (V4 - V2) / 45115)%>%
  summarise(ho = mean(ho)) # 0.2715673
post_het <- read.table("slim/output_vcf/ch1/chr/1.het")%>%
  mutate(ho = (V4 - V2) / 32887)%>%
  summarise(ho = mean(ho)) # 0.3467705
