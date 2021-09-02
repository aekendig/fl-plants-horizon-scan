#### goals: clean and trim plant data for horizon scan ####


#### data: CABI FL Horizon Scan ####

# A full species list that CABI sent to Susan Canavan. This dataset includes all species in their database that are not found in Florida and either have a potential pathway to arrive in Florida or are included in their invasive species compendium.


#### data: Taxonomic Name Resolution Service ####

# I put the CABI plant species list into this website (https://urldefense.proofpoint.com/v2/url?u=http-3A__tnrs.iplantcollaborative.org_&d=DwIGAg&c=sJ6xIWYx-zLMB3EPkvcnVg&r=HjPcW3eohEbbkb1_EzN7HHqRD9KQKShycu2tAMx9e8I&m=-NBGbCgVEI0-gtS1sVfo_AK5pxSmLJRDKZxNQSyzQpM&s=u-1eku7Qbl8lfXV2ezOdjeLkDdNnQ8f1ijy6vBnCsqI&e= )

## used default settings except for matching: 
# processing mode: perform name resolution
# classification (for standardization above genus to family): APGIIII
# sources (descending order): TPL, GCC, ILDIS, TROPICOS, USDA
# match accuracy: no fuzzy or partial matching
# download all matches, detailed
# analysis performed on December 11, 2019


#### data: Atlas of Florida Plants ####

# Downloaded data from https://urldefense.proofpoint.com/v2/url?u=https-3A__florida.plantatlas.usf.edu_browse_county&d=DwIGAg&c=sJ6xIWYx-zLMB3EPkvcnVg&r=HjPcW3eohEbbkb1_EzN7HHqRD9KQKShycu2tAMx9e8I&m=-NBGbCgVEI0-gtS1sVfo_AK5pxSmLJRDKZxNQSyzQpM&s=Lle2Ev1kgwqY9jYa7P8GTUOkctWPGjTFNRnmhxDU9xE&e=  on December 11, 2019
# selected all counties and browse
# chose "Any" for the record format


#### data: Integrated Taxonomic Information System ####

# Accessed this database through the rOpenSci package taxize (see code)


#### data: GloNAF ####

# GloNAF 1.2 database
# Downloaded from https://urldefense.proofpoint.com/v2/url?u=https-3A__idata.idiv.de_DDM_Data_ShowData_257&d=DwIGAg&c=sJ6xIWYx-zLMB3EPkvcnVg&r=HjPcW3eohEbbkb1_EzN7HHqRD9KQKShycu2tAMx9e8I&m=-NBGbCgVEI0-gtS1sVfo_AK5pxSmLJRDKZxNQSyzQpM&s=WNVhY8fWyWOVKVi3vjK_w5eFatm4t_v6YKrnkHK8OyE&e=  on December 11, 2019
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
# # this search misses one species from my manual search on The Plant List (https://urldefense.proofpoint.com/v2/url?u=http-3A__www.theplantlist.org_&d=DwIGAg&c=sJ6xIWYx-zLMB3EPkvcnVg&r=HjPcW3eohEbbkb1_EzN7HHqRD9KQKShycu2tAMx9e8I&m=-NBGbCgVEI0-gtS1sVfo_AK5pxSmLJRDKZxNQSyzQpM&s=xY931O5opKmaYUzCTE3xVLOOAAhWXB_BmEIW-tchbJE&e= ) on December 11, 2019

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


#### synonyms according to Atlas ####

# import data
synonyms_atlas <- read.csv("data/PlantAtlasDataExport-20191211-194219.csv")
# read_csv has issues with the first few rows having NA's

# accepted names
atlas_acc <- synonyms_atlas %>%
  filter(Type == "Accepted Name") %>%
  select(Scientific_Name, X.Plant_ID) %>%
  rename(name = Scientific_Name) %>%
  mutate(accepted = 1,
         accepted_name = name) %>%
  as_tibble()

# check for duplicates
atlas_acc %>%
  filter(duplicated(name) == T)
# none

# check for NA's
atlas_acc %>%
  filter(is.na(name))
# none

# synonyms
# add accepted name
# indicate names with direct association to CABI list
atlas_syn <- synonyms_atlas %>%
  filter(Type == "Synonym")  %>%
  select(Scientific_Name, X.Plant_ID) %>%
  rename(name = Scientific_Name) %>%
  mutate(accepted = 0) %>%
  left_join(select(atlas_acc, X.Plant_ID, accepted_name)) %>%
  mutate(cabi = case_when(name %in% species$Preferred.scientific.name | accepted_name %in% species$Preferred.scientific.name ~ 1,
                                 TRUE ~ 0)) %>%
  as_tibble()

# synonyms with association to CABI
atlas_cabi <- atlas_syn %>%
  filter(cabi == 1)

# update CABI association if the accepted name has an association to CABI (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
# remove species with no association
atlas_syn2 <- atlas_syn %>%
  mutate(cabi = case_when(accepted_name %in% atlas_cabi$accepted_name ~ 1,
                          TRUE ~ cabi)) %>%
  filter(cabi == 1)

# do any of the synonyms match the accepted names?
atlas_syn2 %>%
  filter(name == accepted_name)
# no

# check for duplicates
atlas_syn2 %>%
  filter(duplicated(name) == T)
# 97

# check for NA's
atlas_syn2 %>%
  filter(is.na(name))
# none

atlas_syn2 %>%
  filter(is.na(accepted_name))
# none

# identify duplicate synonym/accepted name pairs
# remove these
# identify duplicate synonyms
atlas_syn3 <- atlas_syn2 %>%
  mutate(dup_syn_acc = duplicated(paste(name, accepted_name, sep = "_"))) %>%
  filter(dup_syn_acc == F) %>%
  mutate(dup_syn = duplicated(name))

# check for duplicates
(atlas_dup <- atlas_syn3 %>%
  filter(dup_syn == T))
# 3

# are any of these synonyms in the CABI list?
atlas_syn3 %>%
  filter(dup_syn == T & name %in% species$Preferred.scientific.name)
# no - remove both cases because we don't know what the accepted name is

# remove duplicated species
atlas_syn4 <- atlas_syn3 %>%
  filter(!(name %in% atlas_dup$name))

# subset accepted names for relevant ones
atlas_acc2 <- atlas_acc %>%
  filter(name %in% atlas_syn4$accepted_name | name %in% species$Preferred.scientific.name)

# make sure lists are unique
(acc_syn_atlas <- atlas_syn4 %>%
  filter(name %in% atlas_acc2$name))
# 3 species
# checked all of these with John Kunzer and they should be kept as separate species (don't keep one as the synonym of the other)

# all 6 species should be in the accepted list
atlas_acc2 %>%
  filter(name %in% acc_syn_atlas$name | name %in% acc_syn_atlas$accepted_name)
# yes

# combine dataframe
# remove unnecessary synonyms
# remove unnecessary columns
atlas_fin <- atlas_syn4 %>%
  filter(!(name %in% atlas_acc2$name)) %>%
  select(name, accepted, accepted_name) %>%
  full_join(select(atlas_acc2, name, accepted, accepted_name)) %>%
  mutate(Atlas = 1) %>%
  mutate(name = as.character(name),
         accepted_name = as.character(accepted_name))


#### synonyms according to ITIS ####

# # get synonyms from ITIS
# synonyms_itis <- synonyms(species$Preferred.scientific.name, db = "itis") 
# # Manual entries needed when multiple records found. Decision hierarchy: scientific name must match the name submitted, if not none of the records were chosen. Chose the records with status "accepted", then chose record with the most information associated with it, then chose the first record listed.

# # convert to dataframe
# # remove V1 (added from species with no matches, all NA's)
# synonyms_itis2 <- rbindlist(lapply(synonyms_itis, as.data.table), use.names = T, fill = T, idcol = "species") %>%
#   select(-V1) 
# head(synonyms_itis2)
# 
# # save ITIS synonyms
# write_csv(synonyms_itis2, "intermediate-data/cabi_full_list_plants_itis_synonyms.csv")

# import back in
synonyms_itis2 <- read_csv("intermediate-data/cabi_full_list_plants_itis_synonyms.csv")

# accepted names
# submitted name matches the accepted name or there's an accepted name for the submitted name
itis_acc <- synonyms_itis2 %>%
  filter(sub_tsn == acc_tsn) %>%
  select(species) %>%
  unique() %>%
  rename(name = species) %>%
  full_join(synonyms_itis2 %>%
              filter(!is.na(acc_name)) %>%
              select(acc_name) %>%
              unique() %>%
              rename(name = acc_name)) %>%
  mutate(accepted = 1,
         accepted_name = name)

# check for duplicates
itis_acc %>%
  filter(duplicated(name) == T)
# none

# check for NA's
itis_acc %>%
  filter(is.na(name))
# none

# synonyms
# submitted name matches the synonym name or there's a synonym for the submitted name
itis_syn <- synonyms_itis2 %>%
  filter(sub_tsn == syn_tsn) %>%
  select(species, acc_name) %>%
  unique() %>%
  rename(name = species,
         accepted_name = acc_name) %>%
  full_join(synonyms_itis2 %>%
              filter(!is.na(syn_name)) %>%
              select(syn_name, acc_name) %>%
              unique() %>%
              rename(name = syn_name,
                     accepted_name = acc_name)) %>%
  mutate(accepted = 0) %>%
  mutate(cabi = case_when(name %in% species$Preferred.scientific.name | accepted_name %in% species$Preferred.scientific.name ~ 1,
                          TRUE ~ 0))

# accepted names with association to CABI
itis_cabi <- itis_syn %>%
  filter(cabi == 1)

# update CABI association if the accepted name has an association
# remove species with no association
itis_syn2 <- itis_syn %>%
  mutate(cabi = case_when(accepted_name %in% itis_cabi$accepted_name ~ 1,
                          TRUE ~ cabi)) %>%
  filter(cabi == 1)

# do any of the synonyms match the accepted names?
(acc_syn_itis_match <- itis_syn2 %>%
  filter(name == accepted_name))
# 6

# are these in the accepted dataset?
itis_acc %>%
  filter(name %in% acc_syn_itis_match$name)
# yes - remove from synonym list

# check for duplicates
itis_syn2 %>%
  filter(duplicated(name) == T)
# 149

# check for NA's
itis_syn2 %>%
  filter(is.na(name))
# none

itis_syn2 %>%
  filter(is.na(accepted_name))
# 2815

# remove name == acc
# remove NA acc
# identify duplicate synonym/accepted name pairs
# remove these
# identify duplicate synonyms
itis_syn3 <- itis_syn2 %>%
  filter(name != accepted_name & !is.na(accepted_name)) %>%
  mutate(dup_syn_acc = duplicated(paste(name, accepted_name, sep = "_"))) %>%
  filter(dup_syn_acc == F) %>%
  mutate(dup_syn = duplicated(name))
  
# check for duplicates
(itis_dup <- itis_syn3 %>%
  filter(dup_syn == T))
# 1

# are any of these synonyms in the CABI list?
itis_syn3 %>%
  filter(dup_syn == T & name %in% species$Preferred.scientific.name)
# no - remove

# remove duplicated species
itis_syn4 <- itis_syn3 %>%
  filter(!(name %in% itis_dup$name))

# subset accepted names for relevant ones
itis_acc2 <- itis_acc %>%
  filter(name %in% itis_syn4$accepted_name | name %in% species$Preferred.scientific.name)

# make sure lists are unique
(acc_syn_itis <- itis_syn4 %>%
    filter(name %in% itis_acc2$name))
# 1 species
# keep separate:
# https://plants.usda.gov/home/plantProfile?symbol=LIDUD
# https://plants.usda.gov/home/plantProfile?symbol=LIPR5

# both species should be in the accepted list
itis_acc2 %>%
  filter(name %in% acc_syn_itis$name | name %in% acc_syn_itis$accepted_name)
# yes

# combine dataframe
# remove unnecessary synonyms
# remove unnecessary columns
itis_fin <- itis_syn4 %>%
  filter(!(name %in% itis_acc$name)) %>%
  select(name, accepted, accepted_name) %>%
  full_join(select(itis_acc2, name, accepted, accepted_name)) %>%
  mutate(ITIS = 1)


#### synonyms according to TNRS ####

# import data from TNRS
synonyms_tnrs <- read_tsv("intermediate-data/cabi_full_list_plants_tnrs_synonyms_20200316.txt")
 
# make sure all CABI species were assessed
length(unique(synonyms_tnrs$Name_submitted)) 
# yes, 2128

# look at warnings
unique(synonyms_tnrs$Warnings)
# Ambiguous match

# look at status for species with this warning
filter(synonyms_tnrs, Warnings == "[Ambiguous match]") %>%
  select(Taxonomic_status) %>%
  unique()
# Accepted

# look at examples
synonyms_tnrs %>%
  filter(Warnings == "[Ambiguous match]" & Taxonomic_status == "Accepted") %>%
  select(Name_submitted, Name_matched)
# they look exactly the same to me, lots of duplicates though

# look at taxonomic_status
unique(synonyms_tnrs$Taxonomic_status)

# do any of the NA's have accepted names?
synonyms_tnrs %>%
  filter(is.na(Taxonomic_status)) %>%
  select(Accepted_name) %>%
  unique()
# no

# look at some non-accepted statuses
synonyms_tnrs %>%
  filter(!(Taxonomic_status %in% c("Accepted", "Synonym"))) %>%
  select(Accepted_name) %>%
  unique()
# 56 have accepted names

# accepted names
# submitted name matches the accepted name or there's an accepted name for the submitted name
tnrs_acc <- synonyms_tnrs %>%
  filter(Taxonomic_status == "Accepted") %>%
  select(Name_matched) %>%
  unique() %>%
  rename(name = Name_matched) %>%
  full_join(synonyms_tnrs %>%
              filter(Taxonomic_status == "Synonym" & !is.na(Accepted_name)) %>%
              select(Accepted_name) %>%
              unique() %>%
              rename(name = Accepted_name)) %>%
  mutate(accepted = 1,
         accepted_name = name)

# check for duplicates
tnrs_acc %>%
  filter(duplicated(name) == T)
# none

# check for NA's
tnrs_acc %>%
  filter(is.na(name))
# none  

# synonyms
# submitted name matches the synonym name or there's a synonym for the submitted name
tnrs_syn <- synonyms_tnrs %>%
  filter(Taxonomic_status == "Synonym") %>%
  select(Name_matched, Accepted_name) %>%
  unique() %>%
  rename(name = Name_matched,
         accepted_name = Accepted_name) %>%
  mutate(accepted = 0) %>%
  mutate(cabi = case_when(name %in% species$Preferred.scientific.name | accepted_name %in% species$Preferred.scientific.name ~ 1,
                          TRUE ~ 0))

# synonyms with association to CABI
tnrs_cabi <- tnrs_syn %>%
  filter(cabi == 1)

# update CABI association if the accepted name has an association (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
# remove species with no association
tnrs_syn2 <- tnrs_syn %>%
  mutate(cabi = case_when(accepted_name %in% tnrs_cabi$accepted_name ~ 1,
                          TRUE ~ cabi)) %>%
  filter(cabi == 1)

# do any of the synonyms match the accepted names?
(acc_syn_tnrs_match <- tnrs_syn2 %>%
    filter(name == accepted_name))
# 42

# are these in the accepted dataset?
acc_syn_tnrs_match %>%
  filter(!(accepted_name %in% tnrs_acc$name))
# yes - remove from synonyms

# check for duplicates
tnrs_syn2 %>%
  filter(duplicated(name) == T)
# 188

# check for NA's
tnrs_syn2 %>%
  filter(is.na(name))
# none  

tnrs_syn2 %>%
  filter(is.na(accepted_name))
# one

# remove name == acc
# remove NA acc
# identify duplicate synonym/accepted name pairs
# remove these
# identify duplicate synonyms
tnrs_syn3 <- tnrs_syn2 %>%
  filter(name != accepted_name & !is.na(accepted_name)) %>%
  mutate(dup_syn_acc = duplicated(paste(name, accepted_name, sep = "_"))) %>%
  filter(dup_syn_acc == F) %>%
  mutate(dup_syn = duplicated(name))

# check for duplicates
tnrs_syn3 %>%
  filter(name %in% filter(tnrs_syn3, dup_syn == T)$name) 
# 315

# how many are in the CABI list?
tnrs_syn3 %>%
  filter(name %in% filter(tnrs_syn3, dup_syn == T)$name) %>%
  filter(name %in% species$Preferred.scientific.name)
# all of them
# can't resolve all of these - remove because we don't know the accepted name

# resolve duplicates
tnrs_syn4 <- tnrs_syn3 %>%
  filter(!(name %in% filter(tnrs_syn3, dup_syn == T)$name))

# subset accepted names for relevant ones
tnrs_acc2 <- tnrs_acc %>%
  filter(name %in% tnrs_syn4$accepted_name | name %in% species$Preferred.scientific.name)

# make sure lists are unique
(acc_syn_tnrs <- tnrs_syn4 %>%
    filter(name %in% tnrs_acc2$name))
# 398
# too many to resolve - remove all

# remove overlapping species
# merge lists
tnrs_fin <- tnrs_syn4 %>%
  filter(!(name %in% acc_syn_tnrs$name)) %>%
  select(name, accepted, accepted_name) %>%
  full_join(tnrs_acc2 %>%
              filter(!(name %in% acc_syn_tnrs$name))) %>%
  mutate(TNRS = 1)


#### combine synonyms ####

# see how many unique names each list contributes
atlas_fin %>%
  filter(!(name %in% c(itis_fin$name, tnrs_fin$name))) %>%
  nrow()
# 5490

itis_fin %>%
  filter(!(name %in% c(atlas_fin$name, tnrs_fin$name))) %>%
  nrow()
# 859

tnrs_fin %>%
  filter(!(name %in% c(itis_fin$name, atlas_fin$name))) %>%
  nrow()
# 1222


### use Atlas as the first authority when there are accepted name/synonym disagreements ###

# are any accepted names in the Atlas synonym list?
(syn_atlas_acc_itis <- itis_fin %>%
  filter(accepted == 1 & name %in% filter(atlas_fin, accepted == 0)$name))
# 26 species
(syn_atlas_acc_tnrs <- tnrs_fin %>%
    filter(accepted == 1 & name %in% filter(atlas_fin, accepted == 0)$name))
# 53 species

# how many synonyms are associated with these names and not in the atlas?
itis_fin %>%
  filter(accepted == 0 & accepted_name %in% syn_atlas_acc_itis$accepted_name & !(name %in% atlas_fin$name))
# 18
tnrs_fin %>%
  filter(accepted == 0 & accepted_name %in% syn_atlas_acc_tnrs$accepted_name & !(name %in% atlas_fin$name))
# 1
# add the Atlas accepted name to these

# extract Atlas names and accepted names
atlas_acc_rep <- atlas_fin %>%
  filter(name %in% c(syn_atlas_acc_itis$accepted_name, syn_atlas_acc_tnrs$accepted_name)) %>%
  rename(atlas_accepted_name = accepted_name, accepted_name = name) %>%
  select(accepted_name, atlas_accepted_name)

# merge with Atlas names
# replace current accepted names
# remove the accepted name row
itis_fin2 <- itis_fin %>%
  left_join(atlas_acc_rep) %>%
  mutate(accepted_name = case_when(!is.na(atlas_accepted_name) ~ atlas_accepted_name,
                                   TRUE ~ accepted_name)) %>%
  filter(!(accepted == 1 & name != accepted_name))

tnrs_fin2 <- tnrs_fin %>%
  left_join(atlas_acc_rep) %>%
  mutate(accepted_name = case_when(!is.na(atlas_accepted_name) ~ atlas_accepted_name,
                                   TRUE ~ accepted_name)) %>%
  filter(!(accepted == 1 & name != accepted_name))

# are any synonyms in the Atlas accepted list?
(acc_atlas_syn_itis <- itis_fin2 %>%
    filter(accepted == 0 & name %in% atlas_fin$accepted_name))
# 7 species
(acc_atlas_syn_tnrs <- tnrs_fin2 %>%
    filter(accepted == 0 & name %in% atlas_fin$accepted_name))
# no species

# how many accepted names are associated with these names and not in the atlas?
itis_fin2 %>%
  filter(accepted == 0 & name %in% acc_atlas_syn_itis$name & !(accepted_name %in% atlas_fin$name))
# 2

# are these in earlier atlas versions?
synonyms_atlas %>%
  filter(Scientific_Name %in% c("Nephrolepis brownii", "Tristagma uniflorum"))
# N. brownii is - assume is a separate species
# T. uniflorum can be a synonym of I. uniflorum: https://www.calflora.org/cgi-bin/namestatus.cgi?taxon=Ipheion+uniflorum&aflag=all

# is N. brownii in cabi?
species %>%
  filter(Preferred.scientific.name == "Nephrolepis brownii")
# no

# are I. uniflorum or T. uniflorum in cabi?
species %>%
  filter(Preferred.scientific.name %in% c("Tristagma uniflorum", "Ipheion uniflorum"))
# just I. uniflorum

# are the accepted names included on their own?
itis_fin2 %>%
  filter((accepted == 1 & accepted_name %in% acc_atlas_syn_itis$accepted_name) | accepted_name %in% c("Nephrolepis brownii", "Tristagma uniflorum"))
# just the two that are not in the Atlas list
# additional synonyms

# remove synonyms that should be accepted names (these will be added from Atlas)
# remove all rows with T. uniflorum as the accepted name
itis_fin3 <- itis_fin2 %>%
  filter(!(name %in% acc_atlas_syn_itis$name | accepted_name == "Tristagma uniflorum"))


### use ITIS as the authority relative to TNRS ###

# are any accepted names in the ITIS synonym list?
(syn_itis_acc_tnrs <- tnrs_fin2 %>%
    filter(accepted == 1 & name %in% filter(itis_fin3, accepted == 0)$name))
# 42 species

# how many synonyms are associated with these names and not in ITIS?
tnrs_fin2 %>%
  filter(accepted == 0 & accepted_name %in% syn_itis_acc_tnrs$accepted_name & !(name %in% itis_fin3$name))
# none

# extract ITIS names and accepted names
itis_acc_rep <- itis_fin3 %>%
  filter(name %in% syn_itis_acc_tnrs$accepted_name) %>%
  rename(itis_accepted_name = accepted_name, accepted_name = name) %>%
  select(accepted_name, itis_accepted_name)

# merge with ITIS names
# replace current accepted names
# remove the accepted name row
tnrs_fin3 <- tnrs_fin2 %>%
  left_join(itis_acc_rep) %>%
  mutate(accepted_name = case_when(!is.na(itis_accepted_name) ~ itis_accepted_name,
                                   TRUE ~ accepted_name)) %>%
  filter(!(accepted == 1 & name != accepted_name))

# are any synonyms in the ITIS accepted list?
(acc_itis_syn_tnrs <- tnrs_fin3 %>%
    filter(accepted == 0 & name %in% itis_fin3$accepted_name))
# 2 species

# how many accepted names are associated with these names and not in the atlas?
tnrs_fin3 %>%
  filter(accepted == 0 & name %in% acc_itis_syn_tnrs$name & !(accepted_name %in% itis_fin3$name))
# 2

# are the accepted names included on their own?
tnrs_fin3 %>%
  filter(accepted_name %in% c("Brachiaria eruciformis", "Reynoutria x bohemica"))
# yes, but don't have any other synonyms

# are the accepted names in the CABI list?
species %>%
  filter(Preferred.scientific.name %in% c("Brachiaria eruciformis", "Reynoutria x bohemica"))
# No

# remove synonyms that should be accepted names (these will be added from Atlas)
# remove the associated accepted names
tnrs_fin4 <- tnrs_fin3 %>%
  filter(!(accepted_name %in% c("Brachiaria eruciformis", "Reynoutria x bohemica")))


### Atlas > ITIS > TNRS for naming ###

# remove ITIS names in Atlas
itis_fin4 <- filter(itis_fin3, !(name %in% atlas_fin$name))

# remove TNRS names in Atlas
tnrs_fin5 <- filter(tnrs_fin4, !(name %in% atlas_fin$name))

# remove TNRS names in ITIS
tnrs_fin6 <- filter(tnrs_fin5, !(name %in% itis_fin4$name))


### combine datasets ###

# look at each
atlas_fin
itis_fin4
tnrs_fin6


# combine
# remove unnecessary columns
# replace NA's in source column with 0
syn_fin <- atlas_fin %>%
  full_join(itis_fin4 %>% select(-atlas_accepted_name)) %>%
  full_join(tnrs_fin6 %>% select(-c(atlas_accepted_name, itis_accepted_name))) %>%
  mutate(Atlas = replace_na(Atlas, 0),
         ITIS = replace_na(ITIS, 0),
         TNRS = replace_na(TNRS, 0))

# look at duplicates
(syn_dup <- syn_fin %>%
    filter(duplicated(name) == T))
# none

# make sure all accepted names have their own row
syn_fin %>%
  select(accepted_name) %>%
  unique() %>%
  rename(name = accepted_name) %>%
  anti_join(syn_fin %>% select(name))
# yes, all are in the "name" column
syn_fin %>%
  select(accepted_name) %>%
  unique() %>%
  nrow
# 2014 accepted names
sum(syn_fin$accepted)
# confirmed


#### add any missing CABI species ####

# remove the duplicate species
# fill in NA rows
syn_fin2 <- syn_fin %>%
  full_join(plants %>%
              select(Preferred.scientific.name) %>%
              rename(name = Preferred.scientific.name) %>%
              unique()) %>%
  mutate(Atlas = replace_na(Atlas, 0),
         ITIS = replace_na(ITIS, 0),
         TNRS = replace_na(TNRS, 0),
         accepted_name = case_when(is.na(accepted_name) ~ name,
                                   TRUE ~ accepted_name),
         CABI = case_when(Atlas + ITIS + TNRS == 0 ~ 1,
                          TRUE ~ 0),
         accepted = case_when(name == accepted_name ~ 1,
                              TRUE ~ 0))

# check for duplicates
(syn_fin_dups <- syn_fin2 %>%
    filter(duplicated(name) == T))
# none


#### add climate data ####

# using the CABI dataset, identify species that are in these regions (removes species that are only on the list because they have a transport pathway to FL)
plants_climate <- plants %>%
  rename(climate = "Number.of.presence.records.countries/states.with.matching.climate") %>%
  filter(climate > 0) 

# number of climate-matching species
nrow(plants_climate)
# 1539

# indicate names on synonym list with direct association to new CABI list
syn_fin3 <- syn_fin2 %>%
  mutate(climate = case_when(name %in% plants_climate$Preferred.scientific.name | accepted_name %in% plants_climate$Preferred.scientific.name ~ 1,
                                  TRUE ~ 0))

# synonyms with association to CABI
syn_cabi_climate <- syn_fin3 %>%
  filter(climate == 1)

# update CABI association if the accepted name has an association to CABI (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_fin4 <- syn_fin3 %>%
  mutate(climate = case_when(accepted_name %in% syn_cabi_climate$accepted_name ~ 1,
                                  TRUE ~ climate)) 


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
syn_fin5 <- syn_fin4 %>%
  mutate(naturalized = case_when(name %in% glo_nat$standardized_name | accepted_name %in% glo_nat$standardized_name ~ 1,
                                  TRUE ~ 0))

# synonyms with association to GloNAF
syn_nat <- syn_fin5 %>%
  filter(naturalized == 1)

# update CABI association if the accepted name has an association to GloNAF list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_fin6 <- syn_fin5 %>%
  mutate(naturalized = case_when(accepted_name %in% syn_nat$accepted_name ~ 1,
                                  TRUE ~ naturalized)) 


#### add GBIF data ####

# # test names 
# test_names <- unique(syn_fin6$accepted_name)[1:20]
# # 8 is a subspecies

# match the names 
# gbif_matches <- syn_fin6$name %>%
#   taxize::get_gbifid_(method="backbone", messages = F) %>% # match names to the GBIF backbone to get taxonkeys
#   imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
#   bind_rows() # combine all data.frames into one
# 
# write_csv(gbif_matches, "intermediate-data/gbif_taxonkeys_full_cabi_list_062221.csv")
gbif_matches <- read_csv("intermediate-data/gbif_taxonkeys_full_cabi_list_062221.csv")

# # look at subspecies
# gbif_matches %>%
#   filter(original_sciname == test_names[8])
# # neither is an exact and accepted match
# # our accepted names don't match GBIF's
# # this is the accepted name and it has far fewer occurrences: Centaurea stoebe subsp. australis
# syn_fin6 %>%
#   filter(accepted_name == test_names[8])

# kingdoms
unique(gbif_matches$kingdom)

# filter for exact matches
gbif_matches2 <- gbif_matches %>%
  as_tibble() %>%
  filter(kingdom != "Animalia") %>%
  left_join(syn_fin6 %>%
              select(name, accepted_name) %>%
              rename(original_sciname = name,
                     original_accepted = accepted_name)) %>%
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
write_csv(gbif_matches2, "intermediate-data/gbif_taxonkeys_full_cabi_list_cleaned_062221.csv")

# kingdoms
unique(gbif_matches2$kingdom)
# filter(gbif_matches2, kingdom == "Animalia") %>%
#   select(canonicalname, original_sciname)
# filter(gbif_matches2, original_sciname == "Bougainvillea rugosa") %>%
#   select(scientificname, original_accepted)
# filter(gbif_matches2, original_accepted == "Bougainvillea rugosa") # plant species that's not on GBIF and doesn't have synonyms
filter(gbif_matches2, kingdom == "Chromista") %>%
  select(canonicalname, original_sciname) # all are seaweeds

# multiple matches
gbif_matches2 %>%
  filter(n > 1)
# a lot
# use the higher occurrence values for these

# non-matches
anti_join(syn_fin6 %>% rename(original_sciname = name), gbif_matches2) # 9 non-matches, some are synonyms

# extract usage key
gbif_taxon_keys <- gbif_matches2 %>% 
  pull(usagekey) # get the gbif taxonkeys

# download gbif data
# (gbif_dwld <- occ_download(pred_in("taxonKey", gbif_taxon_keys),
#              pred("hasCoordinate", TRUE),
#              pred("hasGeospatialIssue", FALSE),
#              pred_lte("year", 2019), # backtrack to date that original data were extracted (2020-01-09)
#              format = "SPECIES_LIST", # summary data, not every occurrence
#              user = "aekendig",
#              pwd = "cf8sUKAJd8CUaV",
#              email = "aekendig@gmail.com"))
# tried to use exact date with pred_lte("eventDate", 2020-01-09),
# but only records before 2010 were returned
# taxon key returns all synonyms, which we don't want
# https://www.gbif.org/developer/occurrence#predicates
# remove records without exact matches to taxonkey upon import
# sent above 6/24/21 at 8:23 AM

# check status
# occ_download_meta(gbif_dwld)

# overview of downloads
# gbif_downloads <- occ_download_list(user = "aekendig",
#                                     pwd = "cf8sUKAJd8CUaV")
# gbif_download_results <- tibble::as_tibble(gbif_downloads$results)

# import download
# occ_download_get(gbif_download_results$key[1], overwrite = T) # downloads zip file to directory
# the parsing of the csv (which is actually tab-delimited) in Excel can mess up the data
# open with TextEdit and save as a .txt file
# open .txt file in Excel (should parse correctly) and resave as a .csv with name below
gbif_output <- read_csv("intermediate-data/gbif_download_full_cabi_list_062421.csv")

# clean dataset
gbif_output2 <- gbif_output %>%
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
  anti_join(gbif_output)
# 5683 missing data
# first 10 have no occurrences or all occurrences have geospatial issues

# original method starts here: 
# code below used to select 100 species for rapid risk assessment
# this method doesn't save the data or create a DOI
# we repeated the data extraction above while writing manuscript to obtain a DOI

# search all synonyms
# gbif <- occ_search(scientificName = syn_fin6$name,
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
syn_fin7 <- syn_fin6 %>%
  left_join(gbif) %>%
  left_join(gbif_output2 %>%
              select(name, occurrences_new)) %>%
  mutate(occurrences_new = replace_na(occurrences_new, 0))

# compare original method (occurrences) with DOI method (occurrences_new)
syn_fin7 %>%
  ggplot(aes(occurrences, occurrences_new)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(formula = y ~ x, method = "lm")

filter(syn_fin7, is.na(occurrences)) # 0


#### add GCW data ####

# import data
gcw <- read.csv("intermediate-data/GCW_full_list_020420_trimspace.csv") 

# number of species
nrow(gcw) 
# 24,601

# indicate names on synonym list with direct association to GCW
syn_fin8 <- syn_fin7 %>%
  mutate(weedy = case_when(name %in% gcw$species | accepted_name %in% gcw$species ~ 1,
                                 TRUE ~ 0))

# synonyms with association to GCW
syn_weed <- syn_fin8 %>%
  filter(weedy == 1)

# update CABI association if the accepted name has an association to GCW list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_fin9 <- syn_fin8 %>%
  mutate(weedy = case_when(accepted_name %in% syn_weed$accepted_name ~ 1,
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
syn_fin10 <- syn_fin9 %>%
  mutate(name_genus = word(name, 1),
         accepted_genus = word(accepted_name, 1),
         fed_nox = case_when(name %in% fed_nox2$name ~ 1,
                             accepted_name %in% fed_nox2$name ~ 1,
                             name_genus %in% fed_nox2_genus$name ~ 1,
                             accepted_genus %in% fed_nox2_genus$name ~ 1,
                             TRUE ~ 0))

# synonyms with association to Fed list
syn_fed_nox <- syn_fin10 %>%
  filter(fed_nox == 1)

# update CABI association if the accepted name has an association to Fed list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
syn_fin11 <- syn_fin10 %>%
  mutate(fed_nox = case_when(accepted_name %in% syn_fed_nox$accepted_name ~ 1,
                           TRUE ~ fed_nox)) 

# create name in FL list
# leave listed and exception - neither of these should be on our list
fl_nox2 <- fl_nox %>%
  mutate(name = case_when(species == "spp." ~ genus,
                          !is.na(subspecies) ~ paste(genus, species, subspecies, sep = " "),
                          species != "spp." & is.na(subspecies) ~ paste(genus, species, sep = " ")))

fl_nox2_genus = fl_nox2 %>%
  filter(species == "spp.")

# indicate names on synonym list with direct association to FL list
syn_fin12 <- syn_fin11 %>%
  mutate(fl_nox = case_when(name %in% fl_nox2$name ~ 1,
                             accepted_name %in% fl_nox2$name ~ 1,
                             name_genus %in% fl_nox2_genus$name ~ 1,
                             accepted_genus %in% fl_nox2_genus$name ~ 1,
                             TRUE ~ 0))

# synonyms with association to FL list
syn_fl_nox <- syn_fin12 %>%
  filter(fl_nox == 1)

# update CABI association if the accepted name has an association to FL list (i.e., one of its synonyms is in the species list, but the accepted name itself is not)
# remove the genus columns
syn_fin13 <- syn_fin12 %>%
  mutate(fl_nox = case_when(accepted_name %in% syn_fl_nox$accepted_name ~ 1,
                             TRUE ~ fl_nox)) %>%
  select(-c(name_genus, accepted_genus))

# number on lists
sum(syn_fin13$fed_nox)
sum(syn_fin13$fl_nox)


#### check final dataset ####

# number of accepted species
length(unique(syn_fin13$accepted_name)) # 2360

# number in original list
syn_plant <- filter(syn_fin13, name %in% plants$Preferred.scientific.name)
length(unique(syn_plant$accepted_name)) # 2071

# look at new names added to list
syn_new <- filter(syn_fin13, !(accepted_name %in% syn_plant$accepted_name))
length(unique(syn_new$accepted_name)) # 289
# checked against final list and none are included 


#### summarize data by accepted species name ####

# check NA's
sum(is.na(syn_fin13$accepted_name))
sum(is.na(syn_fin13$climate))
sum(is.na(syn_fin13$naturalized))
sum(is.na(syn_fin13$occurrences))
sum(is.na(syn_fin13$weedy))
sum(is.na(syn_fin13$fed_nox))
sum(is.na(syn_fin13$fl_nox))

# summarize
acc_fin <- syn_fin13 %>%
  group_by(accepted_name) %>%
  summarize(synonyms = paste(name, collapse = ", "),
            climate = as.numeric(sum(climate) > 0),
            naturalized = as.numeric(sum(naturalized) > 0),
            Atlas = as.numeric(sum(Atlas) > 0),
            occurrences = sum(occurrences), # note that this potentially combines different varieties
            occurrences_new = sum(occurrences_new),
            weedy = as.numeric(sum(weedy) > 0),
            fed_nox = as.numeric(sum(fed_nox) > 0),
            fl_nox = as.numeric(sum(fl_nox) > 0))
# 2360 species


#### trim 1: species that occur in regions with climates similar to FL ####

acc_trim1 <- acc_fin %>%
  filter(climate == 1)
# 1504 species


#### trim 2: species that are not in the FL Atlas ####

acc_trim2 <- acc_trim1 %>%
  filter(Atlas == 0)
# 1307 species


#### trim 3: species not on a noxious weed list ####

acc_trim3 <- acc_trim2 %>%
  filter(fed_nox == 0 & fl_nox == 0)
# 1250 species


#### trim 4: species that are known to be naturalized outside of their native range ####

acc_trim4 <- acc_trim3 %>%
  filter(naturalized == 1)
# 912 species


#### trim 5: species that are in the literature for being weedy ####

acc_trim5 <- acc_trim4 %>%
  filter(weedy == 1)
# 808 species


#### trim 6: expert opinion - remove genus-only and already assessed (John Kunzer) ####

# import John's notes on the previously exported list
jk <- read_csv("intermediate-data/trimmed_list_all_occurrences_20200225_JK.csv") %>%
  select(accepted_name, jk_notes)

# remove the relevant species
acc_trim6 <- acc_trim5 %>%
  mutate(words = str_count(accepted_name, "\\w+")) %>%
  filter(words > 1) %>%
  left_join(jk) %>%
  filter(is.na(jk_notes) | jk_notes != "Removal recommended, based on prior workup by JK. This species (cultivated parsnip) is a temperate-obligate biennial; widely cultivated and has demonstrated minimal capacity to escape cultivation in our climate (one record from LA & one from SC)")
# 806 species


#### trim 7: sort by commonness ####

acc_trim7 <- acc_trim6 %>%
  arrange(desc(occurrences)) %>%
  mutate(synonyms = case_when(synonyms == accepted_name ~ "",
                              TRUE ~ synonyms),
         jk_notes = replace_na(jk_notes, "")) %>%
  select(accepted_name, synonyms, occurrences, jk_notes)


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
  select(accepted_name, occurrences) %>%
  inner_join(occurrences_new_100 %>%
              select(accepted_name, occurrences_new))
# 90 overlapping species

occurrences_100 %>%
  select(accepted_name, occurrences) %>%
  anti_join(occurrences_new_100 %>%
               select(accepted_name, occurrences_new)) %>%
  left_join(acc_trim6 %>%
              select(accepted_name, occurrences_new))
# 10 species in evaluated list have relatively high new occurrences

occurrences_new_100 %>%
  select(accepted_name, occurrences_new) %>%
  anti_join(occurrences_100 %>%
              select(accepted_name, occurrences)) %>%
  left_join(acc_trim6 %>%
              select(accepted_name, occurrences))
# 10 species not evaluated have very similar estimates to occurrences besides one