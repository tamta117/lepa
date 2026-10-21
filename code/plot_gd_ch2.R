gd <- read.table("slim/output/ch2/log/gd.txt", header = T)%>%
  filter(n > 1)%>%
  mutate(tick = tick - 1)

# let's try pivoting
pivot <- pivot_longer(gd, cols = 7:9, names_to = "metrics", values_to = "values")
ggplot(pivot, aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m), linetype = factor(sup)), method = "loess", span = 0.7)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("ho" = "H[o]", "pi" = "pi", "roh" = "F[ROH]"), label_parsed)))+
  scale_linetype_discrete(labels = c("Every year", "Every 2 years"))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Migrant per year",
       fill = "Migrant per year",
       linetype = "Supplement frequency")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))


options(scipen = 999)
ggplot(gd)+
  geom_boxplot(aes(factor(tick), pi))+
  facet_wrap(~m)
pi <- ggplot(gd)+
  geom_smooth(aes(tick, pi, group = factor(m), color = factor(m), fill = factor(m)), method = "loess", span = 0.7)
ggplot(gd)+
  geom_boxplot(aes(factor(tick), ho))+
  facet_wrap(~m)
ho <- ggplot(gd)+
  geom_smooth(aes(tick, ho, group = factor(m), color = factor(m), fill = factor(m)), method = "loess", span = 0.7)
ggplot(gd)+
  geom_boxplot(aes(factor(tick), roh))+
  facet_wrap(~m)               
roh <- ggplot(gd)+
  geom_smooth(aes(tick, roh, group = factor(m), color = factor(m), fill = factor(m)), method = "loess", span = 0.7)



