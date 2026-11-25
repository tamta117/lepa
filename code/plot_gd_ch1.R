library(tidyverse)

# read data
gd <- read.table("slim/output/ch1/log/gd.txt", header = T)%>%
  mutate(tick = tick - 1,
         migrant = if_else(migrant != "none", paste0(migrant, "%"), migrant))
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
ggplot(subset(pivot, metrics %in% c("ho", "roh") & migrant %in% c("none", "100%")), aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m)), method = "loess", span = 1.3, linetype = 1)+
  facet_wrap(~ metrics + pop, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]"), label_parsed)))+
  scale_x_continuous(breaks = seq(0, 60, by = 10))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated individuals per year per pop",
       fill = "Number of translocated individuals per year per pop")+
  theme(axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.position = "top")

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

# plot migrant x m
ggplot(subset(pivot, pop == "ranch" & metrics != "fst"), aes(tick, values))+
  geom_smooth(aes(color = factor(m), fill = factor(m), linetype = factor(migrant)), method = "loess", span = 1.3)+
  facet_wrap(~ metrics, scales = "free_y", labeller = labeller(metrics = as_labeller(c("fst" = "F[st]", "ho" = "H[o]", "he" = "H[e]", "roh" = "F[ROH]", "laf" = "Local~allele~frequency"), label_parsed)))+
  scale_linetype_manual(values = c("100%" = 1, "50%" = 2, "75%" = 3, "none" = 4))+
  scale_x_continuous(breaks = seq(0, 60, by = 10))+
  guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
         color = guide_legend(override.aes = list(fill = NA)))+
  labs(x = "Year",
       color = "Number of translocated\nindividuals per year per pop",
       fill = "Number of translocated\nindividuals per year per pop",
       linetype = "Translocation strategy")+
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

# for presentation --------------------------------------------------------------------------------------------------------------------------------------------------------------

# load font -- not working on mac
library(showtext)
# font_add_google("Montserrat") -- does not work on mac
font_add(family = "montserrat",
         regular = "fonts/Montserrat/static/Montserrat-Regular.ttf",
         bold = "fonts/Montserrat/static/Montserrat-Light.ttf")
showtext_auto()

# set theme
theme_set(theme_minimal(base_family = "montserrat"))
theme_update(text = element_text(color = "#596337", size = 35),
             axis.title = element_text(size = 40),
             axis.ticks = element_blank(),
             legend.key = element_rect(fill = NA, color = NA),
             legend.position = "top",
             legend.background = element_rect(fill = "#D6D3CE", color = NA),
             plot.background = element_rect(fill = "#D6D3CE", color = "#D6D3CE"),
             panel.background = element_rect(fill = "#D6D3CE", color = "#D6D3CE"),
             panel.grid = element_blank(),
             panel.border = element_rect(colour = "#D6D3CE", fill = NA),
             plot.margin = margin(20, 40, 20, 40))

# ho for m
(ho <- ggplot(subset(pivot, metrics == "ho" & migrant %in% c("none", "100%") & pop == "ranch"), aes(tick, values, color = factor(m), fill = factor(m)))+
    geom_vline(xintercept = seq(0, 50, by = 10), color = "#aba9a5", size = 0.2)+
    geom_segment(data = tibble(y = 0.15, x1 = 0, x2 = 60), aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = F, color = "#aba9a5", size = 0.2)+
    geom_segment(data = tibble(y = 0.20, x1 = 0, x2 = 60), aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = F, color = "#aba9a5", size = 0.2)+
    geom_segment(data = tibble(y = 0.25, x1 = 0, x2 = 60), aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = F, color = "#aba9a5", size = 0.2)+
    geom_segment(data = tibble(y = 0.30, x1 = 0, x2 = 60), aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = F, color = "#aba9a5", size = 0.2)+
    geom_segment(data = tibble(y = 0.35, x1 = 0, x2 = 60), aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = F, color = "#aba9a5", size = 0.2)+
    geom_smooth(aes(color = factor(m), fill = factor(m)), method = "loess", span = 1.3, linetype = 1)+
    scale_y_continuous(breaks = seq(0.15, 0.35, by = 0.05), expand = c(0, 0))+
    scale_x_continuous(breaks = seq(0, 60, by = 10), expand = c(0, 0), limits = c(0, 63.5))+
    coord_cartesian(
      clip = "off",
      ylim = c(0.1, 0.35)
    )+
    guides(linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
           color = guide_legend(override.aes = list(fill = NA)))+
    labs(x = "Year",
         y = expression(Observed ~ heterozygosity ~ (H[o])),
         color = "Number of translocated individuals per year per pop",
         fill = "Number of translocated individuals per year per pop")+
    geom_text_repel(aes(label = after_stat(ifelse(x %in% max(x), as.character(color), NA_character_))),
                    stat = "smooth", method = "loess", span = 1.3,
                    family = "montserrat", size = 10, direction = "y", xlim = c(65, NA), hjust = 0.5, vjust = 0, # adjust text
                    segment.size = 0.8, segment.alpha = 1, segment.linetype = "dotted", segment.angle = 0, # adjust segments
                    box.padding = 0.2, show.legend = F))
ggsave("figures/ch1_m.png", dpi = 300, width = 12.59, height = 7, units = "in")

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
