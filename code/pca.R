# identify dispersers and mixed ancestry
library(tidyverse)

# run pca
system(paste0("plink --bfile filter/maf/maf0.1_thin_wild --pca --out filter/maf/maf0.1_thin_wild"))

# read in output
pca <- read.table("filter/maf/maf0.1_thin_wild.eigenvec")

# plot
ggplot(pca, aes(V3, V4, color = V1))+
  geom_point()+
  ggrepel::geom_text_repel(aes(label = V2), size = 3)+
  labs(x = "PC1",
       y = "PC2",
       color = "Origin")+
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.9),
        legend.background = element_rect(fill = "grey100"))
ggsave("figures/pca_wild.png", dpi = 300)

# dispersers in refuge: E32M, LAO04M-24, OM341, OM331, LO03M
# mixed ancestry in refuge: LAO05M-25, LAO02M-25, LO01F, LO02M-22
# mixed ancestry in ranch: E35M, E40M

# write subset files
ranch <- filter(pca, V2 %in% c("E35M", "E40M"))[1:2] %>%
  write.table("odata/ranch_dispersers.txt", col.names = F, row.names = F, quote = F, sep = "\t")
refuge <- filter(pca, V2 %in% c("LAO05M_25", "LAO02M_25", "LO01F", "LO02M_22", "E32M", "LAO04M_24", "OM341", "OM331", "LO03M"))[1:2]%>%
  write.table("odata/refuge_dispersers.txt", col.names = F, row.names = F, quote = F, sep = "\t")

# subset files
system("plink2 --bfile filter/maf/maf0.1_thin_ranch --remove odata/ranch_dispersers.txt --make-bed --export vcf-4.2 --out filter/maf/maf0.1_thin_ranch_nodis")
system("plink2 --bfile filter/maf/maf0.1_thin_refuge --remove odata/refuge_dispersers.txt --make-bed --export vcf-4.2 --out filter/maf/maf0.1_thin_refuge_nodis")
