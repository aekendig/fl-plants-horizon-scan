#### goals: clean and trim plant data for horizon scan ####


#### data: CABI FL Horizon Scan ####

# A full species list that CABI sent to Susan Canavan. This dataset includes all species in their database that are not found in Florida and either have a potential pathway to arrive in Florida or are included in their invasive species compendium.


#### data: Taxonomic Name Resolution Service ####

# I put the CABI plant species list into this website (https://tnrs.biendata.org/)

## used default settings except for matching: 
# processing mode: perform name resolution
# classification (for standardization above genus to family): APGIIII
# sources (descending order): TPL, GCC, ILDIS, TROPICOS, USDA
# match accuracy: no fuzzy or partial matching
# download all matches, detailed
# analysis performed on December 11, 2019


#### data: Atlas of Florida Plants ####

# Downloaded data from https://florida.plantatlas.usf.edu/browse/county on December 11, 2019
# selected all counties and browse
# chose "Any" for the record format


#### data: Integrated Taxonomic Information System ####

# Accessed this database through the rOpenSci package taxize (see code)


#### data: GloNAF ####

# GloNAF 1.2 database
# Downloaded from https://idata.idiv.de/DDM/Data/ShowData/257 on December 11, 2019
# Re-saved this file as a csv becuase it was saved as UTF-16


#### data: Global Compendium of Weeds ####
# copied and pasted from http://www.hear.org/gcw/scientificnames/allscinames.htm on February 4, 2020


#### data: Noxious weed lists ####
# retrieved the following from their websites on the indicated dates
# Federal Noxious Weed List (March 16, 2020)
# https://www.aphis.usda.gov/plant_health/plant_pest_info/weeds/downloads/weedlist.pdf (updated 3/21/17)
# FL Noxious Weed List (March 9, 2020)
# https://www.flrules.org/gateway/ChapterHome.asp?Chapter=5b-57
# https://www.flrules.org/gateway/RuleNo.asp?id=5B-64.011
# FLEPPC List (March 9, 2020)
# http://bugwoodcloud.org/CDN/fleppc/plantlists/2019/2019_Plant_List_ABSOLUTE_FINAL.pdf
# did some manual copying and pasting to get them in the right format


#### plant name database processing ####

## steps taken to process Atlas, ITIS, TNRS
# 1. extract accepted names
# 2. check for duplicates
# 3. check for NA's
# 4. extract synonyms with corresponding accepted names
# 5. check for synonyms that are the same as their accepted names
# 6. check for duplicates
# 7. check for NA's
# 8. remove synonyms that are the same as their accepted names, duplicate synonym/accepted name pairs, and synonyms with NA for accepted names
# 9. identify and resolve duplicate synonyms with different accepted names
# 10. for Atlas, select accepted names that are related to the CABI dataset (other lists are already specific to the CABI dataset)
# 11. identify and resolve overlap between the synonym and accepted name list


#### set-up ####

# clear environment
rm(list = ls())

# packages
library(tidyverse)
library(taxize) # classification function
library(rgbif) # occ_search function
library(data.table) # rbindlist function
library(janitor)
library(keyring) # set up your GBIF username and password


#### extract plants from CABI list ####

# import data
cabi <- read_csv("data/cabi_list_full.csv") 

# edit column names
cabi2 <- cabi
colnames(cabi2) <- str_replace_all(colnames(cabi), " ", ".")

# check taxonomic groups
(cabi_na <- filter(cabi2, is.na(Taxonomic.group)))


#### check uncategorized species ####

# uncat_itis <- classification(cabi_na$Preferred.scientific.name, db = "itis")
# head(uncat_itis)
# do.call(rbind, uncat_itis) %>%
  # rownames_to_column("species") %>%
  # filter(name == "Plantae") 
# # this search misses one species from my manual search on The Plant List (http://www.theplantlist.org/) on December 11, 2019

# uncat_gbif <- classification(cabi_na$Preferred.scientific.name, db = "gbif") 
# # manual entries needed when multiple records found - chose the records with status ACCEPTED and the scientific name most closely matched to the name submitted
# head(uncat_gbif)
# uncat_gbif2 <- do.call(rbind, uncat_gbif) %>%
#   rownames_to_column("species") 
# 
# write_csv(uncat_gbif2, "intermediate-data/cabi_full_list_na_taxa_gbif.csv")
uncat_gbif2 <- read_csv("intermediate-data/cabi_full_list_na_taxa_gbif.csv")

uncat_gbif2 %>%
  filter(name == "Plantae")
# this search captures all expected plants

# list uncategorized plants so that above code doesn't need to be re-run
uncat_gbif_plants <- c("Cucumis dipsaceus", "Nassella poeppigiana", "Pontederia rotundifolia", "Sedum gracile")

# filter for CABI plants and add unknowns
# reformat taxonomic group and phylum to avoid merging errors
plants <- cabi_na %>%
  filter(Preferred.scientific.name %in% uncat_gbif_plants) %>%
  mutate(Taxonomic.group = as.character(Taxonomic.group),
         Phylum = as.character(Phylum)) %>%
  full_join(cabi2 %>%
               filter(Taxonomic.group == "Plants"))

# extract plant species list
species <- plants %>%
  select(Preferred.scientific.name)

# number of species
length(unique(species$Preferred.scientific.name)) 
# 2128

# length of list
nrow(species) 
# two species are repeated

# export this list for the Taxonomic Name Resolution Service (TNRS)
# write_tsv(species, "/intermediate-data/cabi_full_list_plants.txt", col_names = F)


#### accepted names according to Atlas ####

# import data
atlas_out <- read.csv("data/PlantAtlasDataExport-20191211-194219.csv")
# read_csv has issues with the first few rows having NA's

# accepted names
atlas_acc <- atlas_out %>%
  filter(Type == "Accepted Name") %>%
  select(Scientific_Name, X.Plant_ID) %>%
  rename(name = Scientific_Name) %>%
  mutate(accepted = 1,
         acc_name = name) %>%
  as_tibble()

# check for duplicates
atlas_acc %>%
  get_dupes(name)
# none

# check for NA's
atlas_acc %>%
  filter(is.na(name))
# none

# synonyms
# add accepted name
atlas_syn <- atlas_out %>%
  filter(Type == "Synonym")  %>%
  select(Scientific_Name, X.Plant_ID) %>%
  rename(name = Scientific_Name) %>%
  mutate(accepted = 0) %>%
  left_join(select(atlas_acc, X.Plant_ID, acc_name)) %>%
  as_tibble() %>%
  unique() # some rows are duplicated

# check for duplicates
atlas_syn %>%
  get_dupes(name)
# some synonyms have multiple accepted names

# check for NA's
atlas_syn %>%
  filter(is.na(name))
# none

# combine
atlas_all <- atlas_acc %>%
  full_join(atlas_syn)

# select for names in CABI list
atlas_cabi <- atlas_all %>%
  filter(name %in% species$Preferred.scientific.name)

# check for duplicate names
(atlas_cabi_dup_name <- atlas_cabi %>%
  get_dupes(name))
# in all cases, the submitted name is an accepted name and also a synonym of another accepted name
# use the submitted name as the accepted name

atlas_cabi2 <- atlas_cabi %>%
  filter(!(name %in% atlas_cabi_dup_name$name & accepted == 0))

# check for accepted names
atlas_cabi2 %>%
  filter(is.na(acc_name))


#### accepted names according to ITIS ####

# # get synonyms from ITIS
# itis_out <- synonyms(species$Preferred.scientific.name, db = "itis") 
# # Manual entries needed when multiple records found. Decision hierarchy: scientific name must match the name submitted, if not, none of the records were chosen. Chose the records with status "accepted", then chose record with the most information associated with it, then chose the first record listed.

# # convert to dataframe
# # remove V1 (added from species with no matches, all NA's)
# itis_out2 <- rbindlist(lapply(itis_out, as.data.table), use.names = T, fill = T, idcol = "species") %>%
#   select(-V1) 
# head(itis_out2)
# 
# # save ITIS synonyms
# write_csv(itis_out2, "intermediate-data/cabi_full_list_plants_itis_synonyms.csv")

# import back in
itis_out2 <- read_csv("intermediate-data/cabi_full_list_plants_itis_synonyms.csv")

# accepted species with no synonyms returned 0 rows
# species that were not found have an NA in sub_tsn
itis_mis <- species %>%
  filter(!(Preferred.scientific.name %in% itis_out2$species) & 
           !(Preferred.scientific.name %in% itis_out2$acc_name)) %>%
  rename(name = Preferred.scientific.name)
# manually checked first 10 names to confirm that they're accepted

# submitted name matches the accepted name
itis_acc <- itis_out2 %>%
  filter(sub_tsn == acc_tsn) %>% # NA values for sub_tsn are omitted
  select(species) %>%
  unique() %>%
  rename(name = species) %>%
  full_join(itis_mis) %>%
  mutate(accepted = 1,
         acc_name = name)

# check for duplicates
itis_acc %>%
  get_dupes(name)
# none

# check for NA's
itis_acc %>%
  filter(is.na(name))
# none

# synonyms
# submitted name matches the synonym name
itis_syn <- itis_out2 %>%
  filter(sub_tsn == syn_tsn) %>% # NA values for sub_tsn are omitted
  select(species, acc_name) %>%
  unique() %>%
  rename(name = species) %>%
  mutate(accepted = 0)

# check for duplicates
itis_syn %>%
  get_dupes(name)

# check for NA's
itis_syn %>%
  filter(is.na(name))
# none

itis_syn %>%
  filter(is.na(acc_name))
# none

# combine
itis_cabi <- itis_acc %>%
  full_join(itis_syn)

# check for only CABI names
itis_cabi %>%
  filter(!(name %in% species$Preferred.scientific.name))

# check for duplicates
itis_cabi %>%
  get_dupes(name)
# none

# check for accepted names
itis_cabi %>%
  filter(is.na(acc_name))


#### accepted names according to TNRS ####

# import data from TNRS
tnrs_out <- read_tsv("intermediate-data/cabi_full_list_plants_tnrs_synonyms_20200316.txt")

# make sure all CABI species were assessed
length(unique(tnrs_out$Name_submitted)) 
# yes, 2128

# look at warnings
unique(tnrs_out$Warnings)
# Ambiguous match

# look at status for species with this warning
filter(tnrs_out, Warnings == "[Ambiguous match]") %>%
  select(Taxonomic_status) %>%
  unique()

# look at examples
tnrs_out %>%
  filter(Warnings == "[Ambiguous match]" & Taxonomic_status %in% c("Accepted", "Synonym")) %>%
  select(Name_submitted, Name_matched) %>%
  unique()
# they look exactly the same to me, lots of duplicates though

# do any of the NA's have accepted names?
tnrs_out %>%
  filter(is.na(Taxonomic_status)) %>%
  select(Accepted_name) %>%
  unique()
# no

# look at some non-accepted statuses
tnrs_out %>%
  filter(!(Taxonomic_status %in% c("Accepted", "Synonym"))) %>%
  select(Accepted_name) %>%
  unique()
# 55 have accepted names

# tnrs returns multiple accepted names or statuses (accepted/synonym) per species
# resolve by choosing accepted with the most sources if multiple options
tnrs_out2 <- tnrs_out %>%
  filter(Taxonomic_status %in% c("Accepted", "Synonym")) %>%
  group_by(Name_matched, Taxonomic_status, Accepted_name) %>%
  summarize(Source = paste(unique(Source), collapse = ";")) %>% # sources per accepted name
  ungroup() %>%
  rowwise() %>%
  mutate(Source = paste(unique(str_split(Source, ";")[[1]]), collapse = ";")) %>% # remove duplicates
  ungroup() %>%
  mutate(sources = str_count(Source, ";") + 1, # number of sources that agree on accepted name
         name_accepted = if_else(Name_matched == Accepted_name, 1, 0)) %>%
  group_by(Name_matched) %>%
  mutate(acc_names = n_distinct(Accepted_name), # number of accepted names
         max_sources = max(sources), # highest support for an accepted name
         name_accepted_all = as.numeric(sum(name_accepted) > 0)) %>% # is the name accepted by any source?
  ungroup() %>%
  filter(acc_names == 1 | (acc_names > 1 & sources == max_sources)) %>% # choose accepted names with most sources
  group_by(Name_matched) %>%
  mutate(acc_names = n_distinct(Accepted_name)) %>% # recalculate accepted names
  ungroup() %>%
  filter(acc_names == 1 | (acc_names > 1 & name_accepted == 1) | (acc_names > 1 & name_accepted_all == 0)) %>% # choose the CABI name, if accepted
  group_by(Name_matched) %>%
  mutate(acc_names = n_distinct(Accepted_name)) %>% # recalculate accepted names
  ungroup()
  
# remaining duplicates
n_distinct(filter(tnrs_out2, acc_names > 1)$Name_matched)
# 41 taxa with multiple accepted names

# sources
unique(filter(tnrs_out2, acc_names > 1)$Source)

# prioritize sources
tnrs_res <- tnrs_out2 %>%
  filter(acc_names > 1) %>%
  select(Name_matched, Accepted_name, Source) %>%
  pivot_wider(names_from = Source,
              values_from = Accepted_name) %>%
  rename_with(str_replace_all, pattern = ";", replacement = "_") %>%
  unnest(c(gcc, tropicos, tpl, usda, tpl_tropicos, tpl_usda)) %>%
  mutate(Accepted_name = case_when(!is.na(tropicos) ~ tropicos,
                                   !is.na(tpl_tropicos) ~ tpl_tropicos,
                                   is.na(tropicos) & !is.na(tpl) ~ tpl,
                                   is.na(tpl_tropicos) & !is.na(tpl_usda) ~ tpl_usda,
                                   is.na(tropicos) & is.na(tpl) & !is.na(usda) ~ usda,
                                   TRUE ~ gcc),
         Source = case_when(!is.na(tropicos) ~ "tropicos",
                            !is.na(tpl_tropicos) ~ "tpl_tropicos",
                            is.na(tropicos) & !is.na(tpl) ~ "tpl",
                            is.na(tpl_tropicos) & !is.na(tpl_usda) ~ "tpl_usda",
                            is.na(tropicos) & is.na(tpl) & !is.na(usda) ~ "usda",
                            TRUE ~ "gcc")) %>%
  select(Name_matched, Accepted_name, Source) %>%
  unique()

# all names included?
filter(tnrs_res, is.na(Accepted_name))
n_distinct(tnrs_res$Name_matched) # 41

# remaining duplicates that are not in other two sources
(tnrs_res_dupes <- tnrs_res %>%
  get_dupes(Name_matched) %>%
  filter(!(Name_matched %in% atlas_cabi2$name) &
           !(Name_matched %in% itis_cabi$name)))

# manually resolve
tnrs_res2 <- tnrs_res %>%
  mutate(Accepted_name = case_when(Name_matched == "Achillea punctata" ~ "Achillea alpina", # confirmed with tpl
                                   Name_matched == "Aster salignus" ~ "Symphyotrichum × salignum", # confirmed with ITIS, Tropicos, and Wikipedia (looked up common name from CABI)
                                   Name_matched == "Haplophyllum buxbaumii" ~ "Ruta buxbaumii", # confirmed with GBIF (in France)
                                   Name_matched == "Hieracium grandidens" ~ "Hieracium bathycephalum", # traced record from CABI to Seebens et al. 2017
                                   Name_matched == "Hieracium pulmonarioides" ~ "Hieracium amplexicaule subsp. pulmonarioides", # confirmed with GBIF (invasive in Europe)
                                   TRUE ~ Accepted_name),
         Taxonomic_status = if_else(Name_matched == Accepted_name, "Accepted", "Synonym")) %>%
  unique()

# check that all were resolved that needed to be
tnrs_res2 %>%
  get_dupes(Name_matched) %>%
  filter(!(Name_matched %in% atlas_cabi2$name) &
           !(Name_matched %in% itis_cabi$name))

# replace the species with more than one accepted name
tnrs_out3 <- tnrs_out2 %>%
  filter(acc_names == 1) %>%
  full_join(tnrs_res2)

# check number of species
n_distinct(tnrs_out2$Name_matched)
n_distinct(tnrs_out3$Name_matched)

# accepted names
# submitted name matches the accepted name
tnrs_acc <- tnrs_out3 %>%
  filter(Name_matched == Accepted_name) %>%
  select(Name_matched) %>%
  unique() %>%
  rename(name = Name_matched) %>%
  mutate(accepted = 1,
         acc_name = name)

# check for duplicates
tnrs_acc %>%
  get_dupes(name)
# none

# check for NA's
tnrs_acc %>%
  filter(is.na(name))
# none  

# synonyms
# submitted name matches a synonym name
# there are species that are listed as synonyms and accepted names
# use accepted names in case above
tnrs_syn <- tnrs_out3 %>%
  filter(Name_matched != Accepted_name & !(Name_matched %in% tnrs_acc$name)) %>%
  select(Name_matched, Accepted_name) %>%
  unique() %>%
  rename(name = Name_matched,
         acc_name = Accepted_name) %>%
  mutate(accepted = 0)

# check for duplicates
tnrs_syn %>%
  get_dupes(name)
# just Solanum alatum (in another database)

# check for NA's
tnrs_syn %>%
  filter(is.na(name))
# none 

# combine
tnrs_cabi <- tnrs_acc %>%
  full_join(tnrs_syn)

# check for only CABI names
tnrs_cabi %>%
  filter(!(name %in% species$Preferred.scientific.name))

# check for duplicates
tnrs_cabi %>%
  get_dupes(name)
# same as above

# check for accepted names
tnrs_cabi %>%
  filter(is.na(acc_name))


#### resolve accepted names ####

# combine name lists
# prioritize accepted naming
# remove duplicate rows
acc_cabi <- species %>%
  rename(name = Preferred.scientific.name) %>%
  full_join(atlas_cabi2 %>%
              select(-c(X.Plant_ID, accepted)) %>%
              rename(atlas_acc_name = acc_name)) %>%
  full_join(itis_cabi %>%
              select(-accepted) %>%
              rename(itis_acc_name = acc_name)) %>%
  full_join(tnrs_cabi %>%
              select(-accepted) %>%
              rename(tnrs_acc_name = acc_name)) %>%
  mutate(acc_name = case_when(!is.na(atlas_acc_name) ~ atlas_acc_name,
                              is.na(atlas_acc_name) & !is.na(itis_acc_name) ~ itis_acc_name,
                              is.na(atlas_acc_name) & is.na(itis_acc_name) & !is.na(tnrs_acc_name) ~ tnrs_acc_name,
                              TRUE ~ name),
         acc_name_source = case_when(!is.na(atlas_acc_name) ~ "Atlas",
                                     is.na(atlas_acc_name) & !is.na(itis_acc_name) ~ "ITIS",
                                     is.na(atlas_acc_name) & is.na(itis_acc_name) & !is.na(tnrs_acc_name) ~ "TNRS",
                                     TRUE ~ "CABI")) %>%
  select(name, acc_name, acc_name_source) %>%
  unique()
  
# duplicates
acc_cabi %>%
  get_dupes(name)
# none

# number missing from databases
acc_cabi %>%
  filter(acc_name_source == "CABI")
# 141

# accepted names in lower priority databases may be synonyms in higher priority databases
(atlas_syn_check <- acc_cabi %>%
  filter(acc_name_source %in% c("ITIS", "TNRS")) %>%
  select(acc_name, acc_name_source) %>%
  unique() %>%
  rename(name = acc_name) %>%
  inner_join(atlas_all %>% # whole Atlas, not just CABI matches
               filter(name != acc_name) %>%
               select(name, acc_name)) %>%
    rename(acc_name_Atlas = acc_name,
           acc_name = name))
# 5 names

# TNRS names
(itis_syn_check <- acc_cabi %>%
    filter(acc_name_source == "TNRS" & 
             !(acc_name %in% atlas_syn_check$name) & # names resolved above
             !(acc_name %in% itis_acc$acc_name)) %>% # accepted names matched with other CABI names
    select(acc_name) %>%
    unique() %>%
    rename(name = acc_name) %>%
    arrange(name))
# 698 possible

# match with current synonym list?
itis_syn_check %>%
  inner_join(itis_syn)
# none

# get synonyms from ITIS
# manual entries needed when multiple records found. Decision hierarchy: scientific name must match the name submitted, if not, none of the records were chosen. Chose the records with status "accepted", then chose record with the most information associated with it, then chose the first record listed.
# itis_syn_out <- synonyms(itis_syn_check$name, db = "itis")

# convert to dataframe
# remove V1 (added from species with no matches, all NA's)
# itis_syn_out2 <- rbindlist(lapply(itis_syn_out, as.data.table), 
#                            use.names = T, fill = T, idcol = "species") %>%
#   select(-V1)

# save
# write_csv(itis_syn_out2, "intermediate-data/itis_synonym_check.csv")

# re-import
itis_syn_out2 <- read_csv("intermediate-data/itis_synonym_check.csv")

# check for synonyms
# accepted names return 0 rows
# not found have NA for sub_tsn
(itis_syn_check2 <- itis_syn_out2 %>%
    filter(sub_tsn != acc_tsn) %>%
    mutate(acc_name_ITIS = acc_name,
           acc_name = species,
           acc_name_source = "TNRS") %>%
    select(acc_name, acc_name_source, acc_name_ITIS) %>%
    unique())

# combine new accepted names
syn_check <- atlas_syn_check %>%
  full_join(itis_syn_check2)

acc_cabi %>%
  inner_join(syn_check)

# update names with higher level accepted name
acc_cabi2 <- acc_cabi %>%
  left_join(syn_check) %>%
  mutate(acc_name = case_when(!is.na(acc_name_Atlas) ~ acc_name_Atlas,
                              !is.na(acc_name_ITIS) ~ acc_name_ITIS,
                              TRUE ~ acc_name),
         Atlas = if_else(acc_name_source == "Atlas" | !is.na(acc_name_Atlas), 1, 0)) %>%
  select(-c(acc_name_Atlas, acc_name_ITIS))

# names with multiple accepted names
acc_cabi2 %>%
  group_by(name) %>%
  summarize(acc_names = n_distinct(acc_name)) %>%
  ungroup() %>%
  filter(acc_names > 1)
# none

# multiple names grouped together
acc_cabi2 %>%
  group_by(acc_name) %>%
  summarize(names = n_distinct(name)) %>%
  ungroup() %>%
  group_by(names) %>%
  count() %>%
  ungroup() %>%
  mutate(names_tot = names * n)


#### add synonyms from Atlas and ITIS ####

# Atlas synonyms
atlas_all

# ITIS synonyms from original import
itis_orig <- itis_out2 %>% # synonyms associated with accepted names
  filter(sub_tsn == acc_tsn) %>%
  select(syn_name, species) %>%
  rename(acc_name = species) %>%
  full_join(itis_out2 %>% # accepted names themselves
              filter(sub_tsn == acc_tsn) %>%
              select(species) %>%
              rename(acc_name = species) %>%
              mutate(syn_name = acc_name)) %>%
  full_join(itis_out2 %>% # accepted names and their synonyms associated with synonyms
              filter(sub_tsn != acc_tsn) %>%
              select(syn_name, acc_name) %>%
              rename(acc_name = acc_name)) %>%
  unique() %>%
  rename(name = syn_name)

# ITIS synonyms from TNRS accepted
itis_tnrs <- itis_syn_out2 %>% # synonyms associated with accepted names
  filter(sub_tsn == acc_tsn) %>%
  select(syn_name, species) %>%
  rename(acc_name = species) %>%
  full_join(itis_syn_out2 %>% # accepted names themselves
              filter(sub_tsn == acc_tsn) %>%
              select(species) %>%
              rename(acc_name = species) %>%
              mutate(syn_name = acc_name)) %>%
  full_join(itis_syn_out2 %>% # accepted names and their synonyms associated with synonyms
              filter(sub_tsn != acc_tsn) %>%
              select(syn_name, acc_name)) %>%
  unique() %>%
  rename(name = syn_name)

# combine synonyms
syn_cabi <- acc_cabi2 %>%
  select(name, acc_name) %>%
  full_join(atlas_all %>%
              select(-c(X.Plant_ID, accepted))) %>%
  full_join(itis_orig) %>%
  full_join(itis_tnrs) %>%
  inner_join(acc_cabi2 %>% # only include accepted names in list
               select(acc_name) %>%
               unique()) %>%
  unique()

# check that all names are included
n_distinct(acc_cabi2$acc_name)
n_distinct(syn_cabi$acc_name)
acc_cabi2 %>%
  filter(!(name %in% syn_cabi$name))


#### add climate data ####

# using the CABI dataset, identify species that are in these regions (removes species that are only on the list because they have a transport pathway to FL)
plants_climate <- plants %>%
  rename(climate = "Number.of.presence.records.countries/states.with.matching.climate",
         name = "Preferred.scientific.name") %>%
  select(name, climate) %>%
  mutate(climate = if_else(climate > 0, 1, 0)) %>%
  unique()

# number of climate-matching species
sum(plants_climate$climate)
# 1538

# indicate names on synonym list with direct association to climate match
acc_cabi3 <- acc_cabi2 %>%
  left_join(plants_climate)

# synonyms with climate
acc_cabi3 %>%
  filter(climate == 1)

# missing values
acc_cabi3 %>%
  filter(is.na(climate))


#### add GLONAF data ####

# import data
glo <- read_csv("data/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology_121119.csv") 

# subset for naturalized species (alien = unknown if established)
glo_nat <- filter(glo, status == "naturalized") %>%
  select(standardized_name) %>% 
  unique()

# number of species
nrow(glo_nat) 
# 13,083

# indicate names on synonym list with direct association to GloNAF
syn_cabi2 <- syn_cabi %>%
  mutate(naturalized = case_when(name %in% glo_nat$standardized_name | acc_name %in% glo_nat$standardized_name ~ 1,
                                  TRUE ~ 0))

# synonyms with association to GloNAF
syn_nat <- syn_cabi2 %>%
  filter(naturalized == 1)

# update CABI association if the accepted name has an association to GloNAF list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_cabi3 <- syn_cabi2 %>%
  mutate(naturalized = case_when(acc_name %in% syn_nat$acc_name ~ 1,
                                  TRUE ~ naturalized)) 


#### add GCW data ####

# import data
gcw <- read.csv("intermediate-data/GCW_full_list_020420_trimspace.csv") 

# number of species
nrow(gcw) 
# 24,601

# indicate names on synonym list with direct association to GCW
syn_cabi4 <- syn_cabi3 %>%
  mutate(weedy = case_when(name %in% gcw$species | acc_name %in% gcw$species ~ 1,
                                 TRUE ~ 0))

# synonyms with association to GCW
syn_weed <- syn_cabi4 %>%
  filter(weedy == 1)

# update CABI association if the accepted name has an association to GCW list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_cabi5 <- syn_cabi4 %>%
  mutate(weedy = case_when(acc_name %in% syn_weed$acc_name ~ 1,
                                 TRUE ~ weedy)) 


#### add noxious weed lists ####

# import data 
fed_nox <- read_csv("intermediate-data/federal_noxious_weed_list.csv")
fl_nox <- read_csv("intermediate-data/fl_prohibited_plants.csv")

# edit Federal list to remove author names
fed_nox2 <- fed_nox %>%
  mutate(genus = word(name, 1),
         species = word(name, 2))

fed_nox2_genus = fed_nox2 %>%
  filter(species == "spp.")

# indicate names on synonym list with direct association to Fed list
syn_cabi6 <- syn_cabi5 %>%
  mutate(name_genus = word(name, 1),
         acc_genus = word(acc_name, 1),
         fed_nox = case_when(name %in% fed_nox2$name ~ 1,
                             acc_name %in% fed_nox2$name ~ 1,
                             name_genus %in% fed_nox2_genus$name ~ 1,
                             acc_genus %in% fed_nox2_genus$name ~ 1,
                             TRUE ~ 0))

# synonyms with association to Fed list
syn_fed_nox <- syn_cabi6 %>%
  filter(fed_nox == 1)

# update CABI association if the accepted name has an association to Fed list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_cabi7 <- syn_cabi6 %>%
  mutate(fed_nox = case_when(acc_name %in% syn_fed_nox$acc_name ~ 1,
                           TRUE ~ fed_nox)) 

# create name in FL list
# leave listed and exception - neither of these should be on our list
fl_nox2 <- fl_nox %>%
  mutate(name = case_when(species == "spp." ~ genus,
                          !is.na(subspecies) ~ paste(genus, species, subspecies, sep = " "),
                          species != "spp." & is.na(subspecies) ~ paste(genus, species, sep = " ")))

fl_nox2_genus <- fl_nox2 %>%
  filter(species == "spp.")

# indicate names on synonym list with direct association to FL list
syn_cabi8 <- syn_cabi7 %>%
  mutate(fl_nox = case_when(name %in% fl_nox2$name ~ 1,
                             acc_name %in% fl_nox2$name ~ 1,
                             name_genus %in% fl_nox2_genus$name ~ 1,
                             acc_genus %in% fl_nox2_genus$name ~ 1,
                             TRUE ~ 0))

# synonyms with association to FL list
syn_fl_nox <- syn_cabi8 %>%
  filter(fl_nox == 1)

# update CABI association if the accepted name has an association to FL list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
# remove the genus columns
syn_cabi9 <- syn_cabi8 %>%
  mutate(fl_nox = case_when(acc_name %in% syn_fl_nox$acc_name ~ 1,
                             TRUE ~ fl_nox)) %>%
  select(-c(name_genus, acc_genus))


#### add GBIF data ####

# match the names 
# gbif_matches <- syn_cabi9$name %>%
#   taxize::get_gbifid_(method="backbone", messages = F) %>% # match names to the GBIF backbone to get taxonkeys
#   imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
#   bind_rows() # combine all data.frames into one
 
# write_csv(gbif_matches, "intermediate-data/gbif_taxonkeys_full_cabi_list_010622.csv")
gbif_matches <- read_csv("intermediate-data/gbif_taxonkeys_full_cabi_list_010622.csv")

# kingdoms
unique(gbif_matches$kingdom)

# unexpected kingdoms
gbif_matches %>%
  filter(kingdom != "Plantae") %>%
  select(original_sciname, matchtype, kingdom) %>%
  unique() %>%
  group_by(kingdom, matchtype) %>%
  count()
# most will be removed with fuzzy matches

# filter for exact matches
gbif_matches2 <- gbif_matches %>%
  as_tibble() %>%
  filter(kingdom != "Animalia") %>%
  left_join(syn_cabi9 %>%
              select(name, acc_name) %>%
              rename(original_sciname = name,
                     original_accepted = acc_name)) %>%
  group_by(original_sciname) %>%
  mutate(acc_matches = sum(status == "ACCEPTED"),
         ext_matches = sum(matchtype == "EXACT"),
         ae_matches = sum(status == "ACCEPTED" & matchtype == "EXACT")) %>%
  ungroup() %>%
  filter((ae_matches > 0 & status == "ACCEPTED" & matchtype == "EXACT") | # exact accepted matches
           (ae_matches == 0 & ext_matches > 0 & matchtype == "EXACT") | # exact matches
           (ae_matches == 0 & ext_matches == 0 & acc_matches > 0 & status == "ACCEPTED") | # accepted matches
           (ae_matches == 0 & ext_matches == 0 & acc_matches == 0)) %>% # any match
  group_by(original_sciname) %>%
  mutate(n = n()) %>%
  ungroup()

# save for later searches
write_csv(gbif_matches2, "intermediate-data/gbif_taxonkeys_full_cabi_list_cleaned_010622.csv")

# kingdoms
unique(gbif_matches2$kingdom)
filter(gbif_matches2, kingdom == "Chromista") %>%
  select(canonicalname, original_sciname) 
# all six are aquatic

# multiple matches
gbif_matches2 %>%
  filter(n > 1)
# a lot
# use the higher occurrence values for these

# non-matches
anti_join(syn_cabi9 %>% rename(original_sciname = name), gbif_matches2) 
# 8 non-matches, two are accepted names

# investigate unmatched accepted names
acc_cabi3 %>%
  filter(name %in% c("Bougainvillea rugosa", "Juncus kraussii x acutus"))
# the hybrid doesn't seem to be an official name
# Bougainvillea rugosa isn't a plant: http://www.marinespecies.org/aphia.php?p=taxdetails&id=117332

filter(acc_cabi3, str_detect(name, "Juncus") == T)
# Junucs acutus is included, which seems to be the invasive species hybridizing with J. kraussi in Australia
# https://www.dpaw.wa.gov.au/images/documents/conservation-management/off-road-conservation/urban-nature/workshops/proceedings_of_the_managing_sharp_rush_juncus_acutus_works.pdf

syn_cabi9 %>%
  filter(acc_name %in% c("Bougainvillea rugosa", "Juncus kraussii x acutus"))
# no synonyms associated with these and they're not naturalized or weedy

# extract usage key
gbif_taxon_keys <- gbif_matches2 %>% 
  pull(usagekey)

# download gbif data
# (gbif_dwld <- occ_download(pred_in("taxonKey", gbif_taxon_keys),
#              pred("hasCoordinate", TRUE),
#              pred("hasGeospatialIssue", FALSE),
#              pred_lte("year", 2019), # backtrack to date that original data were extracted (2020-01-09)
#              format = "SPECIES_LIST", # summary data, not every occurrence
#              user = "aekendig",
#              pwd = key_get("gbif", "aekendig"),
#              email = "aekendig@gmail.com"))
# tried to use exact date with pred_lte("eventDate", 2020-01-09),
# but only records before 2010 were returned
# taxon key returns all synonyms, which we don't want
# https://www.gbif.org/developer/occurrence#predicates
# remove records without exact matches to taxonkey upon import

# check status
# occ_download_meta(gbif_dwld)

# overview of downloads
# gbif_downloads <- occ_download_list(user = "aekendig",
#                                     pwd = key_get("gbif", "aekendig"))
# gbif_download_results <- tibble::as_tibble(gbif_downloads$results)

# import download
# occ_download_get(gbif_download_results$key[1], overwrite = T) # downloads zip file to directory
# the parsing of the csv (which is actually tab-delimited) in Excel can mess up the data
# open with TextEdit and save as a .txt file
# open .txt file in Excel (should parse correctly) and resave as a .csv with name below
gbif_output <- read_csv("intermediate-data/gbif_download_full_cabi_list_010622.csv")

# duplicates per taxon key
(gbif_key_dups <- get_dupes(gbif_output, taxonKey)) 

gbif_key_dups %>%
  select(taxonRank) %>%
  unique()
# all are genera, use max value

gbif_key_dups2 <- gbif_key_dups %>%
  group_by(taxonKey) %>%
  summarize(numberOfOccurrences = max(numberOfOccurrences)) %>%
  ungroup()

# clean dataset
gbif_output2 <- gbif_output %>%
  filter(!(taxonKey %in% gbif_key_dups2)) %>%
  select(taxonKey, numberOfOccurrences) %>%
  full_join(gbif_key_dups2) %>%
  rename(occurrences_new = numberOfOccurrences) %>%
  inner_join(gbif_matches2 %>%  # select exact matches to taxonKeys
               select(usagekey, original_sciname) %>% # add original name
               rename(taxonKey = usagekey,
                      name = original_sciname))

# missing
gbif_matches2 %>%  # select exact matches to taxonKeys
  select(usagekey, original_sciname) %>% # add original name
  rename(taxonKey = usagekey,
         name = original_sciname) %>%
  anti_join(gbif_output2)
# 8178 missing data
# first 10 have no occurrences or all occurrences have geospatial issues

# duplicates
(gbif_name_dups <- gbif_output2 %>%
  get_dupes(name))
# 1274
# these are different authorities

# add together duplicate names
gbif_output3 <- gbif_output2 %>%
  group_by(name) %>%
  summarize(occurrences_new = sum(occurrences_new)) %>%
  ungroup()

# save
write_csv(gbif_output3, "intermediate-data/gbif_cleaned_full_cabi_list_010622.csv")

# original method starts here: 
# code below used to select 100 species for rapid risk assessment
# it was also done with different (more convoluted) taxonomic resolution methods
# this method doesn't save the data or create a DOI
# we repeated the data extraction above while writing manuscript to obtain a DOI

# search all synonyms
# gbif <- occ_search(scientificName = syn_cabi9$name,
#            limit = 0,
#            hasCoordinate = T,
#            hasGeospatialIssue = F,
#            return = "meta") %>%
#   bind_rows(.id = "name") %>%
#   #select(name, count) %>%
#   rename(occurrences = count)

# check for duplicates
# gbif %>%
#   filter(duplicated(name) == T)

# save data
# write_csv(gbif, "intermediate-data/cabi_full_list_plants_occurrences.csv")

# load data to prevent re-running above
gbif <- read_csv("intermediate-data/cabi_full_list_plants_occurrences.csv")

# add data to synonym dataset
syn_cabi10 <- syn_cabi9 %>%
  left_join(gbif) %>%
  left_join(gbif_output3 %>%
              select(name, occurrences_new)) %>%
  mutate(occurrences_new = replace_na(occurrences_new, 0))

# missing occurrences because of change in taxonomic resolution methods?
syn_cabi10 %>%
  filter(is.na(occurrences)) %>%
  ggplot(aes(x = occurrences_new)) +
  geom_histogram()
# 2191 names, most have zero occurrences

syn_cabi10 %>%
  filter(is.na(occurrences) & occurrences_new > 100) %>%
  select(acc_name) %>%
  n_distinct()
# 183 accepted names

# compare original method (occurrences) with DOI method (occurrences_new)
syn_cabi10 %>%
  ggplot(aes(occurrences, occurrences_new)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
# most are close, some have way more occurrences_new
# occurrences_new may be less strict than occurrences


#### summarize by accepted name ####

# check for missing values
syn_cabi10[!complete.cases(syn_cabi10),] %>%
  select(naturalized, weedy, fed_nox, fl_nox, occurrences) %>%
  unique()
# some occurrences missing

# save
write_csv(syn_cabi10, "intermediate-data/horizon_scan_all_names_plant_list_010622")

# summarize
acc_fin <- syn_cabi10 %>%
  group_by(acc_name) %>%
  summarize(synonyms = paste(name, collapse = ", "),
            naturalized = as.numeric(sum(naturalized) > 0),
            weedy = as.numeric(sum(weedy) > 0),
            fed_nox = as.numeric(sum(fed_nox) > 0),
            fl_nox = as.numeric(sum(fl_nox) > 0),
            occurrences = sum(occurrences, na.rm = T), # note that this potentially combines different varieties
            occurrences_new = sum(occurrences_new, na.rm = T)) %>% 
  full_join(acc_cabi3 %>%
              group_by(acc_name) %>%
              summarize(Atlas = as.numeric(sum(Atlas) > 0),
                        climate = as.numeric(sum(climate) > 0)))
# 2071 taxa

# check for missing values
acc_fin[!complete.cases(acc_fin),] %>%
  select(naturalized, weedy, fed_nox, fl_nox, occurrences, Atlas, climate) %>%
  unique()
# none


#### trim 1: species that occur in regions with climates similar to FL ####

acc_trim1 <- acc_fin %>%
  filter(climate == 1)
# 1503 species


#### trim 2: species that are not in the FL Atlas ####

acc_trim2 <- acc_trim1 %>%
  filter(Atlas == 0)
# 1305 species


#### trim 3: species not on a noxious weed list ####

acc_trim3 <- acc_trim2 %>%
  filter(fed_nox == 0 & fl_nox == 0)
# 1244 species


#### trim 4: species that are known to be naturalized outside of their native range ####

acc_trim4 <- acc_trim3 %>%
  filter(naturalized == 1)
# 929 species


#### trim 5: species that are in the literature for being weedy ####

acc_trim5 <- acc_trim4 %>%
  filter(weedy == 1)
# 832 species


#### trim 6: expert opinion - remove genus-only and already assessed (John Kunzer) ####

# import John's notes on the previously exported list
jk <- read_csv("intermediate-data/trimmed_list_all_occurrences_20200225_JK.csv") %>%
  select(accepted_name, jk_notes) %>%
  rename(acc_name = accepted_name)

# remove the relevant species
acc_trim6 <- acc_trim5 %>%
  mutate(words = str_count(acc_name, "\\w+")) %>%
  filter(words > 1) %>% # remove genus only (too broad)
  left_join(jk) %>%
  filter(is.na(jk_notes) | jk_notes != "Removal recommended, based on prior workup by JK. This species (cultivated parsnip) is a temperate-obligate biennial; widely cultivated and has demonstrated minimal capacity to escape cultivation in our climate (one record from LA & one from SC)")
# 830 species


#### trim 7: sort by commonness ####

acc_trim7 <- acc_trim6 %>%
  arrange(desc(occurrences)) %>%
  mutate(synonyms = case_when(synonyms == acc_name ~ "",
                              TRUE ~ synonyms),
         jk_notes = replace_na(jk_notes, "")) %>%
  select(acc_name, synonyms, occurrences, jk_notes)

# save
write_csv(acc_trim7[1:100,], "intermediate-data/horizon_scan_100_plant_list_010622.csv")


#### assign assessors and reviewers ####

# participants
team <- c("Kendig", "Canavan", "Lieurance", "Flory", "Pfingsten", "Anderson", "Gettys", "Gordon", "Iannone", "Kunzer") %>% sort()

# empty team vector
team_ran_100 <- NA

# seed vector
seed <- c(10, 5, 11, 48, 900, 71, 35, 22, 202, 31)

# randomize team
for(i in 1:10){
  
  set.seed(seed[i])
  
  team_ran_100 <- c(team_ran_100, sample(team))
}

# clear the NA
team_ran_100 <- team_ran_100[-1]

# select first 100 species
# randomize rows
# randomly assign team members
set.seed(41)
sub1_100 <- acc_trim7[1:100, ] %>%
  sample_frac() %>%
  mutate(assessor = rep(team, each = 10),
         reviewer = team_ran_100,
         reviewer = case_when(reviewer == assessor ~ team[match(assessor, team) + 1],
                              TRUE ~ reviewer),
         reviewer = replace_na(reviewer, "Anderson"))

# check distribution
sub1_100 %>%
  group_by(assessor) %>%
  summarise(n = n())

sub1_100 %>%
  group_by(reviewer) %>%
  summarise(n = n())


#### compare occurrence sources ####

# occurrences were obtained with the occ_search function
# these were used for sorting the list
# occurrences_new were obtained with the occ_download function
# these were used to obtain a DOI
# did not know a DOI would be needed at the time occ_search was used

# subset data for top 100 occurrences
occurrences_100 <- acc_trim7[1:100,]
occurrences_new_100 <- acc_trim6 %>%
  arrange(desc(occurrences_new)) %>%
  head(n = 100)

# compare datasets
occurrences_100 %>%
  select(acc_name, occurrences) %>%
  inner_join(occurrences_new_100 %>%
              select(acc_name, occurrences_new))
# 89 overlapping species

occurrences_100 %>%
  select(acc_name, occurrences) %>%
  anti_join(occurrences_new_100 %>%
               select(acc_name, occurrences_new)) %>%
  left_join(acc_trim6 %>%
              select(acc_name, occurrences_new))
# 11 species in evaluated list have relatively high new occurrences

occurrences_new_100 %>%
  select(acc_name, occurrences_new) %>%
  anti_join(occurrences_100 %>%
              select(acc_name, occurrences)) %>%
  left_join(acc_trim6 %>%
              select(acc_name, occurrences))
# 11 species not evaluated have very similar estimates to occurrences besides two