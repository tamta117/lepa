# read data
gd2 <- read.table("slim/output/ch1/log/gd_v2.txt", header = T)
gd <- read.table("slim/output/ch1/log/gd.txt", header = T)%>%
  arrange(migrant, m, duration, sup, iter, pop, tick)%>%
  bind_rows(gd2[-c(6, 14)])%>%
  filter(!(m == 2 & tick == 60))
baseline <- gd%>%
  distinct(migrant, iter, m, duration, sup, pop)%>%
  mutate(n = 25,
         he = if_else(pop == "ranch", 0.3537524, 0.3440284),
         ho = if_else(pop == "ranch", 0.3489646, 0.3443545),
         roh = if_else(pop == "ranch", 0.1711197, 0.2151829),
         fst = 0.09219724,
         tick = 0)
all <- bind_rows(gd, baseline)%>%
  mutate(tick_break = cut(tick, breaks = seq(0, 60, by = 5)))
count_tick <- all%>%
  group_by(migrant, m, duration, sup, pop)%>%
  count(tick_break, name = "n_tick")

# let's try pivoting
pivot <- pivot_longer(all, cols = 9:12, names_to = "metrics", values_to = "values")%>%
  left_join(count_tick, by = c("migrant", "m", "duration", "sup", "pop", "tick_break"))

# plot maxage
ggplot(subset(pivot, migrant == "none" & max_repro_age != 5 & m == 0 & metrics %in% c("ho", "roh")), aes(tick, values))+
  geom_smooth(aes(color = factor(max_repro_age), fill = factor(max_repro_age)), method = "loess", span = 1)+
  facet_wrap(~ metrics + pop, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]"), label_parsed)))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Max reproductive age",
       fill = "Max reproductive age")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# find good loess span
ggplot(subset(pivot, metrics %in% c("ho", "roh") & sup == 1), aes(tick, values)) +
  geom_jitter(aes(color = n)) +
  scale_color_viridis_c("turbo")+
  facet_wrap(~ m + metrics + pop, scales = "free_y")+
  geom_smooth(method = "loess", span = 1, color = "tomato", se = F)+
  geom_smooth(method = "loess", span = 1.3, color = "tan4", se = F)+
  geom_smooth(aes(weight = n), method = "loess", span = 1.3, color = "springgreen3", se = F)+ # very bad fit
  geom_smooth(aes(color = factor(m), fill = factor(m)), method = "glm", method.args = list(family = "inverse.gaussian"), color = "turquoise2", se = F)+
  geom_smooth(data = subset(pivot, metrics %in% c("ho", "roh") & sup == 1 & n > 1), method = "loess", span = 1.3, color = "royalblue", se = F)+
  geom_smooth(data = subset(pivot, metrics %in% c("ho", "roh") & sup == 1 & n > 1), aes(weight = n_tick), method = "loess", span = 1.3, color = "magenta", se = F)
# span 1.3 identical to span 1.4 -- let's go with 1.4 for a smoother line
# no weight looks closer to median -- besides, not sure if we should weight anyways since we want to account for uncertainty (aka population fluctuations)

# plot m
ggplot(subset(pivot, metrics %in% c("ho", "roh") & sup == 1), aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m)), method = "loess", span = 1.3, linetype = 1)+
  facet_wrap(~ metrics + pop, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]"), label_parsed)))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year",
       fill = "Number of translocated\nindividuals per year")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# plot n
ggplot(subset(pivot, sup == 1), aes(tick, n, color = factor(m), fill = factor(m)))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "poisson"))+
  facet_wrap(~ pop, scales = "free_y")+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year",
       fill = "Number of translocated\nindividuals per year")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA))

# facet wrap n
ggplot(subset(pivot, sup == 1), aes(tick, n))+
  geom_jitter(alpha = 0.1)+
  geom_smooth(method = "glm", method.args = list(family = "poisson"))+
  facet_wrap(~ m + pop)+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year",
       fill = "Number of translocated\nindividuals per year")+
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
