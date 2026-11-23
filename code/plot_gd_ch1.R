library(tidyverse)

# read data
gd <- read.table("slim/output/ch1/log/gd.txt", header = T)%>%
  mutate(tick = tick - 1)
baseline <- gd%>%
  distinct(migrant, iter, m, sup, pop)%>%
  mutate(n = 25,
         ho = if_else(pop == "ranch", 0.3489646, 0.3443545),
         roh = if_else(pop == "ranch", 0.1711197, 0.2151829),
         fst = 0.09219724,
         laf = if_else(pop == "ranch", 0.9970284, 0.9999242),
         tick = 0)
all <- bind_rows(gd, baseline)

# let's try pivoting
pivot <- pivot_longer(all, cols = 8:11, names_to = "metrics", values_to = "values")

# find good loess span
ggplot(subset(pivot, metrics %in% c("ho", "roh") & n >= 1), aes(tick, values)) +
  geom_jitter(aes(color = n)) +
  scale_color_gradientn(colors = rainbow(7))+
  facet_wrap(~ m + metrics + pop, scales = "free_y", ncol = 4)+
  geom_smooth(method = "loess", span = 1, color = "tomato", se = F)+
  geom_smooth(method = "loess", span = 1.3, color = "tan4", se = F)+ # smoothest
  geom_smooth(aes(weight = n), method = "loess", span = 1.3, color = "springgreen3", se = F)+ # not as smooth as loess span 1.3
  geom_smooth(method = "glm", method.args = list(family = "inverse.gaussian"), color = "turquoise2", se = F)+ # decent fit for ho, horrible for roh
  geom_smooth(data = subset(pivot, metrics %in% c("ho", "roh") & n > 1), method = "loess", span = 1.3, color = "royalblue", se = F)
# all essentially identical except for glm

# plot m
ggplot(subset(pivot, metrics %in% c("ho", "roh")), aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m)), method = "loess", span = 1.3, linetype = 1)+
  facet_wrap(~ metrics + pop, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]"), label_parsed)))+
  scale_x_continuous(breaks = seq(0, 60, by = 10))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year per pop",
       fill = "Number of translocated\nindividuals per year per pop")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot migrant
ggplot(subset(pivot, m %in% c(0, 1) & pop == "ranch" & metrics != "fst"), aes(tick, values))+
  geom_smooth(aes(color = factor(migrant), fill = factor(migrant)), method = "loess", span = 1.3, linetype = 1)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]", "laf" = "Local"), label_parsed)))+
  scale_x_continuous(breaks = seq(0, 60, by = 10))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year per pop",
       fill = "Number of translocated\nindividuals per year per pop")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot n
ggplot(pivot, aes(tick, n, color = factor(m), fill = factor(m)))+
  geom_jitter(alpha = 0.1)+
  geom_smooth(method = "glm", method.args = list(family = "poisson"))+
  facet_wrap(~ pop, scales = "free_y")+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year per pop",
       fill = "Number of translocated\nindividuals per year per pop")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# facet wrap n
ggplot(subset(pivot, sup == 1), aes(tick, n, color = factor(m), fill = factor(m)))+
  geom_jitter(alpha = 0.1)+
  geom_smooth(method = "glm", method.args = list(family = "poisson"))+
  facet_wrap(~ m + pop, ncol = 2)+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year per pop",
       fill = "Number of translocated\nindividuals per year per pop")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# no dispersers -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# read in outputs
nodis <- read.table("slim/output/ch1/log/nodis.txt", header = T)
dis <- read.table("slim/output/ch1/log/rerun_maxage13.txt", header = T)%>%
  filter(m == 0)%>%
  bind_rows(nodis[-14], .id = "dis")

# write baseline gd
baseline <- dis%>%
  distinct(dis, migrant, iter, m, duration, sup, max_repro_age, pop)%>%
  mutate(n = 25,
         he = if_else(pop == "ranch", 0.3537524, 0.3440284),
         ho = if_else(pop == "ranch", 0.3489646, 0.3443545),
         roh = if_else(pop == "ranch", 0.1711197, 0.2151829),
         fst = 0.09219724,
         tick = 0)
all <- bind_rows(dis, baseline)

# let's try pivoting
pivot <- pivot_longer(all, cols = 10:13, names_to = "metrics", values_to = "values")

# plot
ggplot(subset(pivot, metrics %in% c("ho", "roh")), aes(tick, values))+
  geom_smooth(aes(color = factor(dis), fill = factor(dis)), method = "loess", span = 1.2, linetype = 1)+
  facet_wrap(~ metrics + pop, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]"), label_parsed)))+
  guides(color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Dispersers and mixed ancestry",
       fill = "Dispersers and mixed ancestry")+
  scale_fill_discrete(labels = c("Present", "Not present"))+
  scale_color_discrete(labels = c("Present", "Not present"))+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))
