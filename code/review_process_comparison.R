#### set up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)

# import data
hs_pre <- read_csv("intermediate-data/first_round_assessments_050120.csv")
hs_post <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")


#### edit data ####

# remove spaces from column names
colnames(hs_pre) <- make.names(colnames(hs_pre), unique = T)
colnames(hs_post) <- make.names(colnames(hs_post), unique = T)

# edit changed species names
# remove NA score (not given by assessor)
hs_pre2 <- hs_pre %>%
  mutate(Species = case_when(Species == "Sambucus nigra" ~ "Sambucus nigra ssp. nigra",
                             Species == "Rhamnus frangula" ~"Frangula alnus",
                             TRUE ~ Species)) %>%
  filter(!is.na(Overall.score))

# select scores and combine
dat <- hs_pre2 %>%
  select(Species, Overall.score) %>%
  rename(Pre.score = Overall.score) %>%
  inner_join(hs_post %>%
               select(Species, Overall.score) %>%
               rename(Post.score = Overall.score)) %>%
  mutate(Score.diff = Post.score - Pre.score)


#### t-test ####

# visualize comparison
dat %>%
  pivot_longer(cols = c(Pre.score, Post.score),
               names_to = "Timing",
               values_to = "Score") %>%
  ggplot(aes(x = Timing, y = Score)) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0) +
  stat_summary(geom = "point", fun = "mean", size = 2)

# check normality of difference
ggplot(dat, aes(x = Score.diff)) +
  geom_density()
# most scores did not change before/after

ggplot(dat, aes(sample = Score.diff)) +
  stat_qq() + stat_qq_line()

# assume unequal variances
t.test(dat$Pre.score, dat$Post.score, paired = T)

# assume equal variances
t.test(dat$Pre.score, dat$Post.score, paired = T, var.equal = T)
# same result


#### values for text ####

# pre/post averages
dat %>%
  summarise(Mean.pre = mean(Pre.score),
            SE.pre = sd(Pre.score)/sqrt(length(Pre.score)),
            Mean.post = mean(Post.score),
            SE.post = sd(Post.score)/sqrt(length(Post.score)))

# high/medium/low categories
dat2 <- dat %>%
  mutate(Pre.category = case_when(Pre.score >= 64 ~ 3,
                                  Pre.score < 64 & Pre.score >= 27 ~ 2,
                                  Pre.score < 27 ~ 1),
         Post.category = case_when(Post.score >= 64 ~ 3,
                                   Post.score < 64 & Post.score >= 27 ~ 2,
                                   Post.score < 27 ~ 1),
         Category.diff = Post.category - Pre.category)

# changes in category
dat2 %>% count(Category.diff)
dat2 %>% filter(Category.diff > 0)
dat2 %>% filter(Category.diff < 0)
