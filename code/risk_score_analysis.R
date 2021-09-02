#### set up ####

# clear environment
rm(list = ls())

# load packages (restart R session for packages to be loaded in correct order)
library(MASS) # for glm.nb
library(tidyverse)
library(rgbif)
library(lubridate) # for year
library(DHARMa) # for model diagnostics
library(emmeans) # for pairwise comparisons
library(cowplot)

# import data
hs <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")
gbif_keys <- read_csv("intermediate-data/gbif_taxonkeys_full_cabi_list_cleaned_062221.csv")


#### edit data ####

# remove spaces from column names
colnames(hs) <- make.names(colnames(hs), unique = T)

# edit names for gbif (ones that returned zero records)
# assigned combined assessment to one
# put all of Petri's assessments together
# combine freshwater and marine
# make medium certainty the intercept
hs2 <- hs %>%
  mutate(gbif.species = case_when(Species == "Calystegia sepium spp. sepium" ~ "Calystegia sepium subsp. sepium",
                                  Species == "Veronica serpyllifolia ssp. serpyllifolia" ~ "Veronica serpyllifolia subsp. serpyllifolia",
                                  Species == "Sambucus nigra ssp. nigra" ~ "Sambucus nigra var. nigra",
                                  Species == "Frangula alnus" ~ "Rhamnus frangula", # accepted name used during search
                                  TRUE ~ Species),
         Assessor = case_when(Assessor == "Anderson and Kendig" ~ "Anderson",
                              Assessor %in% c("Petri (for Flory)", "Petri (for Gordon)") ~ "Petri",
                              TRUE ~ Assessor),
         Terrestrial = case_when(Environment == "T" ~ 1,
                                 TRUE ~ 0),
         Overall.certainty = fct_relevel(Overall.certainty, c("High", "Medium", "Low")))

# pulled species usage keys in first search
# need subspecies keys
gbif_matches <- c("Calystegia sepium subsp. sepium", 
                  "Veronica serpyllifolia subsp. serpyllifolia",
                  "Sambucus nigra var. nigra") %>%
  taxize::get_gbifid_(method="backbone", messages = F) %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(gbif.species = .y)) %>% # add original name back into data.frame
  bind_rows() # combine all data.frames into one
# don't need to trim list

# add gbif taxon keys
hs_keys <- hs2 %>%
  left_join(gbif_keys %>%
              mutate(gbif.species = case_when(original_sciname == "Campylopus introflexus" ~ "Campylopus introflexus",
                                              TRUE ~ original_accepted)) %>%
              select(gbif.species, usagekey) %>%
              full_join(gbif_matches %>%
                          select(gbif.species, usagekey)))

# check for missing values
filter(hs_keys, is.na(usagekey)) %>%
  select(gbif.species)

length(unique(hs_keys$gbif.species))

# extract usage key
hs_taxon_keys <- hs_keys %>% 
  pull(usagekey) # get the gbif taxonkeys

# download gbif data
# (gbif_dwld <- occ_download(pred_in("taxonKey", hs_taxon_keys),
#              pred("hasCoordinate", TRUE),
#              pred("hasGeospatialIssue", FALSE),
#              pred("country", "US"),
#              format = "SIMPLE_CSV", # need all records to get earliest one
#              user = "aekendig",
#              pwd = "cf8sUKAJd8CUaV",
#              email = "aekendig@gmail.com"))

# check status
# occ_download_meta(gbif_dwld)

# overview of downloads
# gbif_downloads <- occ_download_list(user = "aekendig",
#                                     pwd = "cf8sUKAJd8CUaV")
# gbif_download_results <- tibble::as_tibble(gbif_downloads$results)
# 
# # import download
# occ_download_get(gbif_download_results$key[1], overwrite = T) # downloads zip file to directory

# NOTE: this file was too large to post on Github
# download file from GBIF: https://doi.org/10.15468/dl.qh62zw
# save in intermediate-data folder as hs_US_records_080421.csv

# import data
gbif_output <- read_csv("intermediate-data/hs_US_records_080421.csv",
                        col_types = cols(.default = "c",
                                         gbifID = "d",
                                         individualCount = "d",
                                         decimalLatitude = "d",
                                         decimalLongitude = "d",
                                         coordinateUncertaintyInMeters = "d",
                                         coordinatePrecision = "d",
                                         elevation = "d",
                                         elevationAccuracy = "d",
                                         depth = "d",
                                         depthAccuracy = "d",
                                         eventDate = "T",
                                         day = "d",
                                         month = "d",
                                         year = "d",
                                         taxonKey = "d",
                                         speciesKey = "d",
                                         dateIdentified = "T",
                                         lastInterpreted = "T"))

# summarize by HS taxa
# only use taxa with matching keys
gbif_output2 <- hs_keys %>%
  rename(taxonKey = usagekey) %>%
  select(taxonKey, gbif.species) %>%
  left_join(gbif_output) %>%
  group_by(gbif.species) %>%
  summarise(occurrences = n(),
            earliest = min(eventDate, na.rm = T),
            taxonRank = paste(unique(taxonRank), collapse = ", "),
            iNatOcc = sum(institutionCode == "iNaturalist")) %>%
  ungroup() %>%
  mutate(earliest = case_when(earliest == Inf ~ as.POSIXct("2021-03-23 00:00:00", tz = "UTC"),
                              gbif.species == "Alopecurus pratensis" ~ as.POSIXct("1849-05-01 00:00:00", tz = "UTC"),
                              gbif.species == "Rumex acetosa" ~ as.POSIXct("1862-01-01 00:00:00", tz = "UTC"),
                              gbif.species == "Gnaphalium uliginosum" ~ as.POSIXct("1829-07-01 00:00:00", tz = "UTC"),
                              TRUE ~ earliest),
         iNatOcc = replace_na(iNatOcc, 0))
# warning was addressed
# some have arbitrary dates, reset to next earliest record (manual GBIF search)
# manually checked the ones ending in 0

# combine data
# scale continuous variables
hs3 <- hs2 %>%
  select(Species, Overall.score, Assessor, Overall.certainty, Terrestrial, gbif.species) %>%
  left_join(gbif_output2) %>%
  mutate(earlYear = lubridate::year(earliest),
         earlYearS = scale(earlYear)[,1],
         countS = scale(occurrences)[,1],
         logOcc = log10(occurrences))

# record correlation
cor.test(~ earlYearS + countS, data = hs3)
# not sig


#### model ####

# full model
mod1 <- glm.nb(Overall.score ~ Assessor + Overall.certainty + Terrestrial + earlYearS + countS, data = hs3)
summary(mod1)
# doesn't seem like terrestrial is significant

mod2 <- update(mod1, .~. - Terrestrial)
anova(mod1, mod2)
# okay to remove terrestrial

summary(mod2)
# counts are not sig

mod3 <- update(mod2, .~. - countS)
anova(mod2, mod3)
# remove

summary(mod3)

# test significance of each remaining
mod4 <- update(mod3, .~. -Assessor)
anova(mod3, mod4)

mod5 <- update(mod3, .~. - Overall.certainty)
anova(mod3, mod5)

mod6 <- update(mod3, .~. - earlYearS)
anova(mod3, mod6)


#### diagnostics ####

mod3_res <- simulateResiduals(mod3)
plot(mod3_res)


#### pairwise comparisons ####

# assessor
mod3_assess <- emmeans(mod3, ~ Assessor)
pairs(mod3_assess)

# certainty
mod3_cert <- emmeans(mod3, ~ Overall.certainty)
pairs(mod3_cert)


#### figure settings ####

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

# simulated data
earlRec <- hs3 %>%
  select(earlYear, earlYearS) %>%
  unique() %>%
  mutate(Assessor = "Anderson",
         Overall.certainty = "Medium",
         countS = 0) %>%
  mutate(Overall.score = predict(mod3, newdata = ., type = "response"),
         Overall.score.se = predict(mod3, newdata = ., type = "response", se.fit = T)$se.fit)

# figures
earlYearFig <- ggplot(hs3, aes(earlYear, Overall.score)) +
  geom_point() +
  geom_ribbon(data = earlRec, aes(ymin = Overall.score - Overall.score.se, 
                                  ymax = Overall.score + Overall.score.se),
              alpha = 0.5) +
  geom_line(data = earlRec) +
  geom_text(x = 2021, y = 100, label = expression(paste(italic(P), " = 0.05")), hjust = 1, check_overlap = T, size = 2.5) +
  xlab("Year of earliest record") +
  ylab("Overall risk score") +
  ggtheme

numRecFig <- ggplot(hs3, aes(log10(occurrences + 1), Overall.score)) +
  geom_point() +
  geom_text(x = 5, y = 100, label = expression(paste(italic(P), " = 0.20")), hjust = 1, check_overlap = T, size = 2.5) +
  xlab(expression(paste("Number of records (", log[10], ")", sep = ""))) +
  ylab("Overall risk score") +
  ggtheme

# figure
plot_grid(earlYearFig, numRecFig,
          labels = LETTERS[1:2])
