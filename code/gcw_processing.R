# load packages
library(tidyverse)
library(rapportools)
# this is in a separate script because it masks so many functions

# import data
gcw <- read.csv("data/GCW_full_list_020420.csv") 

# remove spaces
gcw2 <- gcw %>%
  mutate(species = trim.space(species))

# write data
write.csv(gcw2, "intermediate-data/GCW_full_list_020420_trimspace.csv")