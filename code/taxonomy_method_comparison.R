#### set-up ####

# goal: taxa_list_processing methods were revised during manuscript revisions to make sure correct accepted names were used

# clear environment
rm(list = ls())

# packages
library(tidyverse)

# import data
old_list <- read_csv("intermediate-data/first_round_assessments_050120.csv")
new_syns <- read_csv("intermediate-data/horizon_scan_all_names_plant_list_011522")
new_acc <- read_csv("intermediate-data/horizon_scan_accepted_names_plant_list_011522")
glo <- read_csv("data/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology_121119.csv") 
itis_out2 <- read_csv("intermediate-data/cabi_full_list_plants_itis_synonyms.csv")
supp <- read_csv("intermediate-data/table_S1_090221.csv")


#### old list: missing from new synonyms? ####

# add synonyms
old_list_syn <- old_list %>%
  select(Species) %>%
  rename(name = Species) %>%
  left_join(new_syns) %>%
  mutate(accepted = if_else(acc_name == name, 1, 0))

# missing accepted names
old_list_syn %>%
  filter(is.na(acc_name))

# see which name it's associated with
old_list %>% 
  filter(str_detect(Species, "Calystegia") == T | str_detect(Synonyms, "Calystegia") == T) %>%
  select(Species, Synonyms) %>%
  data.frame()
# Calystegia sepium must have been the original name
# the subspecies sepium was chosen because it's the only non-native one according to USDA Plants
# checked on ITIS and subspecies sepium is the accepted name

new_acc %>% 
  filter(str_detect(synonyms, "Calystegia") == T | str_detect(acc_name, "Calystegia") == T)
# Calystegia sepium is on this list and does have high occurrences


#### old list: accepted or synonym? ####
old_list_syn %>%
  filter(accepted == 0)
# Campylopus introflexus: one authority is accepted in ITIS, other isn't
# Campylopus introflexus not naturalized
# Veronica serpyllifolia ssp. serpyllifolia is a subspecies and the accepted name is Veronica serpyllifolia

old_list %>%
  filter(Species %in% c("Campylopus introflexus", "Veronica serpyllifolia ssp. serpyllifolia")) %>%
  select(Species, Synonyms) %>%
  data.frame()
# Dicranum introflexum is a synonym of the accepted one authority
# Veronica serpyllifolia looks like it has same story as Calystegia sepium

glo %>%
  filter(standardized_name %in% c("Campylopus introflexus", "Dicranum introflexum"))
# checked for more synonyms in ITIS, Atlas, and TNRS and didn't find any
# this species was added back into the list because it's non-vascular

itis_out2 %>%
  filter(species == "Campylopus introflexus" | acc_name == "Campylopus introflexus")
# the other accepted name was chosen because the authority with this accepted name returned zero rows
# unclear why synonyms for Campylopus introflexus weren't returned -- user error when manually selecting multiple entries?


#### taxonomic source check ####

# name flagged by reviewer
new_acc %>%
  filter(acc_name %in% c("Salvia rosmarinus", "Rosmarinus officinalis")) %>%
  select(acc_name, synonyms, acc_name_source) %>%
  data.frame()
# the latter is supported by ITIS


#### revise supplemental table ####

# add taxonomic sources
supp2 <- supp %>%
  rename(name = "Name",
         acc_name = "Accepted name") %>%
  left_join(new_syns %>%
              select(name, acc_name, acc_name_source))

# see if any are missing
supp2 %>%
  filter(is.na(acc_name_source))
# 469
# use new_acc as new table

# indicates species assessed
new_acc2 <- new_syns %>%
  inner_join(old_list %>%
              select(Species) %>%
              rename(name = Species) %>%
              mutate(name = if_else(name == "Calystegia sepium spp. sepium", "Calystegia sepium", name),
                     selected = 1)) %>%
  select(acc_name, selected) %>%
  full_join(new_acc) %>%
  mutate(selected = replace_na(selected, 0))

# make sure all info is available for selected
new_acc2 %>%
  filter(selected == 1) %>%
  filter(is.na(acc_name) | is.na(acc_name_source))


# save file
write_csv(new_acc2, "intermediate-data/table_S1_011522.csv")
