#### set up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)

# import data
hs <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")


#### edit data ####

# remove spaces from column names
colnames(hs) <- make.names(colnames(hs), unique = T)

# pathways
paths <- c("release", # original: release in nature
           "escape", # original: escape from confinement
           "contaminant", # original: transport contaminant
           "stowaway", # original: transport-stowaway
           "corridor",
           "unaided")

paths_orig <- tibble(pathway = paths,
                     pathway_orig = c("release in nature",
                                      "escape from confinement", 
                                      "transport contaminant",
                                      "transport stowaway",
                                      "corridor",
                                      "unaided"))

# create new columns to indicate which pathways are applicable
hs2 <- hs %>%
  mutate(Likely.pathway.of.arrival = tolower(Likely.pathway.of.arrival) %>%
           str_replace("contaminent", paths[3]) %>%
           str_replace("containiment", paths[3]) %>%
           str_replace("contamimant", paths[3]) %>%
           str_replace("containment", paths[3]) %>%
           str_replace("contaiminant", paths[3]) %>%
           str_replace("contaminatnt", paths[3]) %>%
           str_replace("contamination", paths[3]) %>%
           str_replace("stoway", paths[4]) %>%
           str_replace("stow away", paths[4]) %>%
           str_replace("intentional introduction", paths[1]) %>%
           str_replace("potentially all modes", paste(paths, collapse = ", ")) %>%
           str_replace("escape from contaminant", paths[2]),
         pathway_release = str_detect(Likely.pathway.of.arrival, paths[1]) %>%
           as.numeric(),
         pathway_escape = str_detect(Likely.pathway.of.arrival, paths[2]) %>%
           as.numeric(),
         pathway_contaminant = str_detect(Likely.pathway.of.arrival, paths[3]) %>%
           as.numeric(),
         pathway_stowaway = str_detect(Likely.pathway.of.arrival, paths[4]) %>%
           as.numeric(),
         pathway_corridor = str_detect(Likely.pathway.of.arrival, paths[5]) %>%
           as.numeric(),
         pathway_unaided = str_detect(Likely.pathway.of.arrival, paths[6]) %>%
           as.numeric())

# look at output for entries that don't fit with this text
hs2 %>%
  select(Likely.pathway.of.arrival, pathway_release:pathway_unaided) %>%
  write_csv(., "intermediate-data/pathways_check.csv")

# long dataset
hs3 <- hs2 %>%
  select(Species, Overall.score, pathway_release:pathway_unaided) %>%
  pivot_longer(cols = -c(Species, Overall.score),
               names_to = "pathway",
               names_prefix = "pathway_",
               values_to = "count") %>%
  mutate(Risk.group = case_when(Overall.score >= 64 ~ "high",
                                Overall.score < 64 & Overall.score >= 27 ~ "medium",
                                Overall.score < 27 ~ "low")) %>%
  group_by(pathway, Risk.group) %>%
  summarise(species = sum(count)) %>%
  ungroup() %>%
  left_join(paths_orig)

# save
write_csv(hs3, "intermediate-data/pathways_summary.csv")

