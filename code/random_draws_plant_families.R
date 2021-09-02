#### set up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(taxize)
library(Taxonstand)

# import data
cabi <- read_csv("data/cabi_list_full.csv")
hs <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")


#### edit The Plant List data ####

# download the plant list CSV files
# tpl_get("data/the-plant-list")
# included in data folder

# extract file/family names
tpl <- tibble(file_name = list.files("data/the-plant-list", pattern = ".csv")) %>%
  mutate(family = str_remove(file_name, ".csv"),
         taxa = NA) %>%
  filter(family != "Isoëtaceae") # remove families with missing data

# add number of names
for(i in 1:nrow(tpl)){
  
  temp <- read_csv(paste0("data/the-plant-list/", tpl$file_name[i])) %>%
    rename(status = "Taxonomic status in TPL") %>%
    filter(status == "Accepted")
  tpl$taxa[i] <- nrow(temp) # includes accepted and unresolved
  
}

# check that it worked
filter(tpl, is.na(taxa))

# expand family list by number of taxa
tpl2 <- tpl %>%
  filter(taxa > 0) %>%
  group_by(family) %>%
  summarise(family_rep = seq(from = 1, to = taxa)) %>%
  ungroup()

# check that it worked
nrow(tpl2) - sum(filter(tpl, taxa > 0)$taxa)
# 0 = yes


#### edit CABI data ####

# edit column names
cabi2 <- cabi
colnames(cabi2) <- str_replace_all(colnames(cabi), " ", ".")

# plants that were not categorized as plants
uncat_gbif_plants <- c("Cucumis dipsaceus", "Nassella poeppigiana", "Pontederia rotundifolia", "Sedum gracile")

# plants only
cabi3 <- cabi2 %>%
  filter(Preferred.scientific.name %in% uncat_gbif_plants) %>%
  full_join(cabi2 %>%
              filter(Taxonomic.group == "Plants")) %>%
  rename(Species = Preferred.scientific.name) %>%
  select(Species, Family, Phylum) %>%
  unique() %>%
  filter(!(Species == "Sedum gracile" & is.na(Family))) %>% # remove duplicate species
  filter(!(Species == "Pontederia rotundifolia" & is.na(Family)))

# tpl families (saved this)
# cabi_tpl <- TPL(splist = cabi3$Species)
# write_csv(cabi_tpl, "intermediate-data/cabi_family_names_080321.csv")
cabi_tpl <- read_csv("intermediate-data/cabi_family_names_080321.csv")
head(cabi_tpl)

# add tpl families
cabi4 <- cabi3 %>%
  rename(Family_CABI = Family) %>%
  left_join(cabi_tpl %>%
              select(Taxon, Family) %>%
              rename(Species = Taxon)) %>%
  mutate(Family = case_when(Family == "" & Family_CABI %in% tpl$family ~ Family_CABI, # add families from CABI
                            Family == "" & Family_CABI == "Asteraceae" ~ "Compositae", # alt name
                            Family == "" & Species == "Cuscuta" ~ "Convolvulaceae", # re-assigned
                            TRUE ~ Family)) %>%
  filter(!(Family == "" & Phylum %in% c("Chlorophyta", "Heterokontophyta", "Phaeophyta", "Rhodophyta", "Streptophyta"))) %>% # remove non-vascular phyla with no family match (TPL is vascular + bryophytes)
  filter(Family != "Isoëtaceae") # can't extract these data from TPL

# check for missing
filter(cabi4, Family == "") %>%
  arrange(Family_CABI) %>%
  data.frame()


#### edit HS list ####

# tpl families (saved this)
# hs_tpl <- TPL(splist = hs$Species)
# write_csv(hs_tpl, "intermediate-data/hs_family_names_080321.csv")
hs_tpl <- read_csv("intermediate-data/hs_family_names_080321.csv")
head(hs_tpl)

# add tpl families
hs2 <- hs %>%
  left_join(hs_tpl %>%
              select(Taxon, Family) %>%
              rename(Species = Taxon)) %>%
  filter(Species != "Aegagropila linnaei") # the other non-vascular is a byrophyte

# missing family names
filter(hs2, is.na(Family) | Family == "") %>%
  select(Species, Synonyms)


#### random samples ####

# samples
cabi_n <- nrow(cabi4)
hs_n <- nrow(hs2)

# families
cabi_fam <- length(unique(cabi4$Family))
hs_fam <- length(unique(hs2$Family))

# adjusted p-values
0.05/cabi_fam # need 10,000
0.05/hs_fam # need 1000

# empty lists
cabi_samp <- list()
hs_samp <- list()

# sample family (output saved below)
for(i in 1:1000){
  
  hs_samp[[i]] <- tibble(family = sample(tpl2$family, size = hs_n)) %>%
    group_by(family) %>%
    count() %>%
    ungroup() %>%
    mutate(draw = i)
}

for(i in 1:10000){
  
  cabi_samp[[i]] <- tibble(family = sample(tpl2$family, size = cabi_n)) %>%
    group_by(family) %>%
    count() %>%
    ungroup() %>%
    mutate(draw = i)
}

# make into dataframe
cabi_samp2 <- bind_rows(cabi_samp) 
hs_samp2 <- bind_rows(hs_samp)

# write_csv(cabi_samp2, "intermediate-data/cabi_family_resamples_080321.csv")
# write_csv(hs_samp2, "intermediate-data/hs_family_resamples_080321.csv")

cabi_samp2 <- read_csv("intermediate-data/cabi_family_resamples_080321.csv")
hs_samp2 <- read_csv("intermediate-data/hs_family_resamples_080321.csv")

# add mean and quantiles
cabi_samp3 <- cabi_samp2 %>%
  full_join(tibble(family = tpl$family) %>% # include every family in every draw
              expand_grid(tibble(draw = 1:10000))) %>%
  mutate(n = replace_na(n, 0)) %>% # add zeros for no species drawn
  group_by(family) %>%
  mutate(mean_n = mean(n)) %>%
  ungroup()

hs_samp3 <- hs_samp2 %>%
  full_join(tibble(family = tpl$family) %>% # include every family in every draw
              expand_grid(tibble(draw = 1:1000))) %>%
  mutate(n = replace_na(n, 0)) %>% # add zeros for no species drawn
  group_by(family) %>%
  mutate(mean_n = mean(n)) %>%
  ungroup()

# visualize to check
cabi_samp3 %>%
  filter(family == "Acanthaceae") %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean_n), color = "red")

hs_samp3 %>%
  filter(family == "Acanthaceae") %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean_n), color = "red")

# compare CABI to samples
cabi5 <- cabi_samp3 %>%
  rename(Family = family) %>%
  inner_join(cabi4 %>%
               group_by(Family) %>%
               count() %>%
               ungroup() %>%
               rename(obs = n)) %>%
  mutate(over = ifelse(n >= obs, 1, 0), # adjusted p-value comparison
         under = ifelse(n <= obs, 1, 0)) %>%
  group_by(Family) %>%
  summarise(num_over = sum(over),
            num_under = sum(under),
            mean_n = unique(mean_n),
            obs = unique(obs)) %>%
  ungroup() %>%
  left_join(tpl %>%
              select(family, taxa) %>%
              rename(Family = family,
                     acc_spp = taxa)) %>%
  select(Family, acc_spp, mean_n, obs, num_over, num_under)

# check that it worked
filter(cabi5, is.na(mean_n))

filter(cabi5, num_over < 3) # 22
filter(cabi5, num_under < 3) # 8

# compare HS to samples
hs3 <- hs_samp3 %>%
  rename(Family = family) %>%
  inner_join(hs2 %>%
               group_by(Family) %>%
               count() %>%
               ungroup() %>%
               rename(obs = n)) %>%
  mutate(over = ifelse(n >= obs, 1, 0), # adjusted p-value comparison
         under = ifelse(n <= obs, 1, 0)) %>%
  group_by(Family) %>%
  summarise(num_over = sum(over),
            num_under = sum(under),
            mean_n = unique(mean_n),
            obs = unique(obs)) %>%
  ungroup() %>%
  left_join(tpl %>%
              select(family, taxa) %>%
              rename(Family = family,
                     acc_spp = taxa)) %>%
  select(Family, acc_spp, mean_n, obs, num_over, num_under)

# check that it worked
filter(hs3, is.na(mean_n))

filter(hs3, num_over <= 1)
filter(hs3, num_under <= 1)

# overlap between lists
filter(cabi5, num_over < 3 & Family %in% filter(hs3, num_over <= 1)$Family)
