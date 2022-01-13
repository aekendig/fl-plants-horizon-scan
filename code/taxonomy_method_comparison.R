
# import data
new_list <- read_csv("intermediate-data/horizon_scan_100_plant_list_010622.csv")
old_list <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")
new_syns <- read_csv("intermediate-data/horizon_scan_all_names_plant_list_010622")

# select names
new_names <- new_list %>%
  select(acc_name, synonyms) %>%
  rename(name = acc_name) %>%
  mutate(new = 1)

old_names <- old_list %>%
  select(Species) %>%
  rename(name = Species) %>%
  mutate(old = 1)

# combine names
all_names <- new_names %>%
  full_join(old_names)

# missing in new list
all_names %>%
  filter(is.na(new))

all_names %>%
  filter(is.na(old))

#### start here ####

# compare
old_names %>%
  anti_join(new_names)
# 15 missing

# in synonyms?
old_syns <- old_names %>%
  left_join(new_syns)

old_syns %>%
  filter(is.na(acc_name))
# 14 not in list

# ones that aren't accepted
old_syns %>%
  filter(name != acc_name)
# 3 taxa
# one is missing its occurrences and the other is not naturalized