#### set up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(wesanderson)
library(cowplot)

# import data
dat <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")
paths <- read_csv("intermediate-data/pathways_summary.csv")


#### edit data ####

# remove spaces from column names
colnames(dat) <- make.names(colnames(dat), unique = T)

# add risk groups
dat2 <- dat %>%
  mutate(Risk.group = case_when(Overall.score >= 64 ~ "high",
                                Overall.score < 64 & Overall.score >= 27 ~ "medium",
                                Overall.score < 27 ~ "low") %>%
           fct_relevel("high", "medium"),
         Overall.certainty = tolower(Overall.certainty) %>%
           fct_relevel("high", "medium"),
         Certainty.star = recode(Overall.certainty,
                                 "high" = "***",
                                 "medium" = "**",
                                 "low" = "*",
                                 "very low" = ""))

# relevel certainty groups
cert <- dat2 %>%
  mutate(Overall.certainty = fct_relevel(Overall.certainty, "very low", "low", "medium")) %>%
  group_by(Overall.certainty) %>%
  summarise(Overall.score.se = sd(Overall.score)/sqrt(length(Overall.score)),
            Overall.score = mean(Overall.score)) %>%
  ungroup() %>%
  mutate(lets = c("bc", "c", "b", "a"),
         letsy = Overall.score + Overall.score.se + 2)

# pathway order
path_order <- paths %>%
  group_by(pathway_orig) %>%
  summarise(tot = sum(species)) %>%
  ungroup() %>%
  arrange(tot)

# relevel risk groups
paths2 <- paths %>%
  mutate(Risk.group = fct_relevel(Risk.group, "high", "medium"),
         pathway_orig = factor(pathway_orig, levels = path_order$pathway_orig))


#### figure settings ####

# color palette
score_pal <- wes_palette("Zissou1", 3, type = "continuous") %>%
  rev()
shade_pal <- c(1, 0.7, 0.3, 0.1)

# text sizes
title_size = 10
text_size = 8

# figure theme
ggtheme <- theme_bw() +
  theme(axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = title_size),
        legend.text = element_text(size = text_size),
        legend.title = element_text(size = text_size,),
        strip.text = element_text(size = title_size),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))


#### figure ####

# certainty-risk relationship
cert_score_fig <- ggplot(cert, aes(Overall.certainty, Overall.score)) +
  geom_bar(fill = "grey30", color = NA, stat = "identity", aes(alpha = Overall.certainty)) +
  geom_errorbar(aes(ymin = Overall.score - Overall.score.se, ymax = Overall.score + Overall.score.se), width = 0.1) +
  geom_text(aes(y = letsy, label = lets), size = 2.5) +
  scale_alpha_manual(values = rev(shade_pal), guide = "none") +
  coord_flip() +
  xlab("Certainty") +
  ylab("Overall risk score") +
  ggtitle("C") +
  ggtheme +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = -0.43))

# pathways
path_fig <- paths2 %>%
  ggplot(aes(x = species, y = pathway_orig, fill = Risk.group)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  ggtitle("B") +
  xlab("Number of taxa") +
  ylab("Likely pathway of arrival") +
  scale_fill_manual(values = score_pal, guide = "none") +
  ggtheme +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = -0.43))

# combined figure
overall_fig <- dat2 %>%
  mutate(ordering = Overall.score*10 - as.numeric(Overall.certainty),
         Species = fct_reorder(Species, ordering)) %>%
ggplot(aes(x = Overall.score,
                 y = Species, 
                 fill = Risk.group,
                 alpha = Overall.certainty)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = score_pal,
                    name = "Risk group") +
  scale_alpha_manual(values = shade_pal,
                     name = "Certainty") +
  ylab("Plant species") +
  xlab("Overall risk score") +
  ggtitle("A") +
  annotation_custom(ggplotGrob(path_fig), xmin = 26, xmax = 100, ymin = 30, ymax = 60) +
  annotation_custom(ggplotGrob(cert_score_fig), xmin = 45, xmax = 100, ymin = 7, ymax = 30) +
  ggtheme +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 5.5, 
                                   color = "black", 
                                   face = "italic"),
        plot.title = element_text(size = 14, face = "bold", hjust = -0.32),
        legend.box = "horizontal",
        legend.position = c(0.75, 0.75))

# figure
overall_fig
