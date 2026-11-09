# post processing for slim output vcf
suppressPackageStartupMessages(library(tidyverse, quietly = T))
suppressPackageStartupMessages(library(genio, quietly = T))

# define constants
plink2 <- "C:/Users/kutlt017/Documents/plink2_win64_20260504/plink2.exe"

# define arguments
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]

# read in data
gt <- read.table(paste0("slim/output_vcf/ch1/", file, "_gt.txt"))
samples <- read.table(paste0("slim/output_vcf/ch1/", file, "_samples.txt"))
old_bim <- read.table("filter/maf/maf0.1_thin.bim")

# write new bim
lost <- anti_join(old_bim, gt, by = c("V1", "V4" = "V2"))
new_bim <- data.frame(V1 = gt$V1,
                      V2 = paste0(gt$V1, "_", gt$V2),
                      V3 = 0,
                      V4 = gt$V2,
                      V5 = gt$V4,
                      V6 = gt$V3)%>%
  bind_rows(lost)%>%
  arrange(V1, V4)
if (n_distinct(new_bim$V5) > 4 | n_distinct(new_bim$V6) > 4) {
  new_bim <- filter(new_bim, V5 %in% c("A", "G", "T", "C") & V6 %in% c("A", "G", "T", "C"))
  gt <- semi_join(gt, new_bim, by = c("V1", "V2" = "V4"))
}
write.table(new_bim, paste0("slim/output_vcf/ch1/", file, ".bim"), row.names = F, col.names = F, quote = F)

# write new fam
new_fam <- data.frame(fid = if_else(grepl("p1", samples$V1), "p1", "p2"),
                      iid = samples$V1,
                      V3 = 0,
                      V4 = 0,
                      V5 = 0,
                      V6 = -9)
write.table(new_fam, paste0("slim/output_vcf/ch1/", file, ".fam"), row.names = F, col.names = F, quote = F)

# write new bed
new_bed <- gt %>%
  bind_rows(select(lost, c(V1, V2 = V4)))%>%
  arrange(V1, V2)%>%
  select(-c(1:4))%>%
  mutate(across(everything(), ~ case_when(is.na(.) ~ "0",
                                          .x == "0|0" ~ "0",
                                          .x == "1|1" ~ "2",
                                          .x == "0|1" | .x == "1|0" ~ "1") |>
                  as.integer(.)))
write_bed(paste0("slim/output_vcf/ch1/", file, ".bed"), as.matrix(new_bed))  

# write new vcf 
system(paste0(plink2, " --bfile slim/output_vcf/ch1/", file, " --chr-set 17 --export vcf-4.2 --out slim/output_vcf/ch1/", file, "_all"))

# extract ranch and refuge
ranch <- filter(new_fam[,1:2], fid == "p1")
refuge <- filter(new_fam[,1:2], fid == "p2")
write.table(ranch, "slim/output_vcf/ch1/ranch.txt", row.names = F, col.names = F, quote = F)
write.table(refuge, "slim/output_vcf/ch1/refuge.txt", row.names = F, col.names = F, quote = F)
system(paste0(plink2, " --bfile slim/output_vcf/ch1/", file, " --chr-set 17 --keep slim/output_vcf/ch1/ranch.txt --make-bed --export vcf-4.2 --out slim/output_vcf/ch1/", file, "_ranch"))
system(paste0(plink2, " --bfile slim/output_vcf/ch1/", file, " --chr-set 17 --keep slim/output_vcf/ch1/refuge.txt --make-bed --export vcf-4.2 --out slim/output_vcf/ch1/", file, "_refuge"))

# write fst pop file
ranch_fst <- ranch%>%
  mutate(fst = paste0(fid, "_", iid))
write.table(select(ranch_fst, fst), "slim/files/ranch_fst.txt", row.names = F, col.names = F, quote = F)
refuge_fst <- refuge%>%
  mutate(fst = paste0(fid, "_", iid))
write.table(select(refuge_fst, fst), "slim/files/refuge_fst.txt", row.names = F, col.names = F, quote = F)
