# range of gd
all50 <- read.table("slim/output/offspring/log/best50_gd.txt", header = T) # roh: 0-0.006, ho: 0.33-0.39
all75 <- read.table("slim/output/offspring/log/best75_gd.txt", header = T) # roh: 0.1-0.3, ho: 0.2-0.32
# 75% has more range -> they still need time to homogenize and for those ROH to propagate throughout the population

# read data
gd <- read.table("slim/output/ch2/log/gd.txt", header = T)%>%
  filter(n > 1)%>%
  mutate(tick = tick - 1)

# let's try pivoting
pivot <- pivot_longer(gd, cols = 10:12, names_to = "metrics", values_to = "values")

# plot m for 50%
ggplot(subset(pivot, f == 50), aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m), linetype = factor(sup)), method = "loess", span = 0.7)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("ho" = "H[o]", "pi" = "pi", "roh" = "F[ROH]"), label_parsed)))+
  scale_linetype_discrete(labels = c("Every year", "Every 2 years"))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Individuals per supplement",
       fill = "Individuals per supplement",
       linetype = "Supplement frequency")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot 50% vs 75%
ggplot(subset(pivot, sup == 1), aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m), linetype = factor(f)), method = "loess", span = 0.7)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("ho" = "H[o]", "pi" = "pi", "roh" = "F[ROH]"), label_parsed)))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Individuals per supplement",
       fill = "Individuals per supplement",
       linetype = "Texas ancestry proportion")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot mk
ggplot(subset(pivot, mk != 3 & m == 1 & sup == 1), aes(tick, values))+
  geom_smooth(aes(color = factor(mk), fill = factor(mk), linetype = factor(f)), method = "loess", span = 0.7)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("ho" = "H[o]", "pi" = "pi", "roh" = "F[ROH]"), label_parsed)))+
  scale_color_discrete(labels = c("Random", "Mean kinship"))+
  scale_fill_discrete(labels = c("Random", "Mean kinship"))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Mating selection strategy",
       fill = "Mating selection strategy",
       linetype = "Texas ancestry proportion")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))
ggplot(subset(pivot, mk != 3 & f == 75 & m == 1 & sup == 1), aes(tick, values))+
  geom_smooth(aes(color = factor(mk), fill = factor(mk)), method = "loess", span = 0.7)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("ho" = "H[o]", "pi" = "pi", "roh" = "F[ROH]"), label_parsed)))+
  scale_color_discrete(labels = c("Random", "Mean kinship", "Opposite homozygous"))+
  scale_fill_discrete(labels = c("Random", "Mean kinship", "Opposite homozygous"))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Mating selection strategy",
       fill = "Mating selection strategy",
       title = "75% Texas ancestry")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot k
ggplot(subset(pivot, mk == 0 & m == 1 & sup == 1 & f == 50), aes(tick, values))+
  geom_smooth(aes(color = factor(k), fill = factor(k)), method = "loess", span = 0.7)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("ho" = "H[o]", "pi" = "pi", "roh" = "F[ROH]"), label_parsed)))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Carrying capacity",
       fill = "Carrying capacity")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot k and n
ggplot(subset(pivot, mk == 0 & m == 1 & sup == 1 & f == 50), aes(factor(tick), n))+
  geom_boxplot()+
  facet_wrap(~k, labeller = as_labeller(c("30" = "K = 30", "60" = "K = 60")))+
  labs(x = "Year",
       y = "Individual count")