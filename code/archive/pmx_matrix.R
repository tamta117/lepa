# kinship matrix for PMx
library(tidyverse)

# run KING matrix
# square0 fills in the NA with 0
system("plink2 --bfile filter/maf/maf0.1_thin_wmzf --make-king square0 --out filter/maf/maf0.1_thin_wmzf")

# read in KING output
id <- read.table("filter/maf/maf0.1_thin_wmzf.king.id")%>%
  select(V1 = V2)
king <- read.table("filter/maf/maf0.1_thin_wmzf.king")%>%
  mutate(across(everything(), ~ if_else(. == 0, NA, if_else(. < 0, 0, .))))

# write matrix and ID
write.table(id, "pmx/king_id.txt", row.names = F, col.names = F, quote = F)
write.table(king, "pmx/king_matrix.txt", row.names = F, col.names = F, quote = F, sep = "\t")
