#### set up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(taxize)
library(rvest)
library(maps)
library(countrycode)

# import data
dat <- read_csv("intermediate-data/horizon_scan_plants_full_reviews_080321.csv")

#### edit data ####

# remove spaces from column names
colnames(dat) <- make.names(colnames(dat), unique = T)

# # retrieve Plants of the World IDs
# pow_ID <- get_pow(dat$Species)
# # manually accepted entries with accepted = T and names that matched input
# # checked website for distribution map if unsure
# 
# # create a dataframe
# dat2 <- tibble(Species = dat$Species,
#                ID = pow_ID[1:99],
#                url = attributes(pow_ID)$uri)
# 
# # manually check missing entries
# filter(dat2, is.na(ID))
# 
# # add missing entries
# dat3 <- dat2 %>%
#   mutate(ID = case_when(Species == "Calystegia sepium spp. sepium" ~ "urn:lsid:ipni.org:names:77172848-1",
#                         Species == "Veronica serpyllifolia ssp. serpyllifolia" ~ "urn:lsid:ipni.org:names:812650-1",
#                         Species == "Sambucus nigra ssp. nigra" ~ "urn:lsid:ipni.org:names:30122169-2",
#                         Species == "Ehrharta stipoides" ~ "urn:lsid:ipni.org:names:408485-1",
#                         TRUE ~ ID),
#          url = case_when(Species == "Calystegia sepium spp. sepium" ~ "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77172848-1",
#                          Species == "Veronica serpyllifolia ssp. serpyllifolia" ~ "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:812650-1",
#                          Species == "Sambucus nigra ssp. nigra" ~ "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30122169-2",
#                          Species == "Ehrharta stipoides" ~ "http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:408485-1",
#                          TRUE ~ url))
# # had to use species instead of subsp. for Veronica serpyllifolia: remove U.S. and Canada from native range
# # used the species instead of subsp. for Sambucus nigra, but range is consistent with CABI description
# # Ehrharta stipoides is considered a synonym for Microlaena stipoides in this database
# # make manual entry using CABI for Campylopus introflexus
# # leave out Aegagropila linnaei, native distribution unclear
# 
# # save IDs
# write_csv(dat3, "intermediate-data/POW_IDs_final_list_20210810.csv")
dat3 <- read_csv("intermediate-data/POW_IDs_final_list_20210810.csv")


#### retrieve distributions ####

# method from: https://github.com/aurielfournier/wilson_ornithological_society_tweets
# use https://selectorgadget.com/ to figure out what to put in html_nodes

# # functions
# pow_native <- function(url){
#   
#   # entries with "doubtful"
#   header1 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing h3:nth-child(3)") %>%
#     html_text
#   
#   output1 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing p:nth-child(4)") %>%
#     html_text %>%
#     str_replace_all("\n", "") %>%
#     str_replace_all(" ", "") %>%
#     str_replace_all(",", ", ")
#   
#   # entries without "doubtful"
#   header2 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing h3:nth-child(1)") %>%
#     html_text
#   
#   output2 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing p:nth-child(2)") %>%
#     html_text %>%
#     str_replace_all("\n", "") %>%
#     str_replace_all(" ", "") %>%
#     str_replace_all(",", ", ")
#   
#   return(list(header1, output1, header2, output2))
# }
# 
# pow_introd <- function(url){
#   
#   # entries with "doubtful"
#   header1 <- read_html(url) %>%
#     html_nodes(css = "h3:nth-child(5)") %>%
#     html_text
#   
#   output1 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing p:nth-child(6)") %>%
#     html_text %>%
#     str_replace_all("\n", "") %>%
#     str_replace_all(" ", "") %>%
#     str_replace_all(",", ", ")
#   
#   # entries without "doubtful" or with "literature"
#   header2 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing h3:nth-child(3)") %>%
#     html_text
#   
#   output2 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing p:nth-child(4)") %>%
#     html_text %>%
#     str_replace_all("\n", "") %>%
#     str_replace_all(" ", "") %>%
#     str_replace_all(",", ", ")
#   
#   # entries with "doubtful" and "extinct"
#   header3 <- read_html(url) %>%
#     html_nodes(css = "h3:nth-child(7)") %>%
#     html_text
#   
#   output3 <- read_html(url) %>%
#     html_nodes(css = "#distribution-listing p:nth-child(8)") %>%
#     html_text %>%
#     str_replace_all("\n", "") %>%
#     str_replace_all(" ", "") %>%
#     str_replace_all(",", ", ")
#   
#   return(list(header1, output1, header2, output2, header3, output3))
# }
# 
# # apply functions
# datNat <- dat3 %>%
#   filter(!is.na(url)) %>%
#   mutate(locations = purrr::map(url, pow_native))
# 
# datIntro <- dat3 %>%
#   filter(!is.na(url)) %>%
#   mutate(locations = purrr::map(url, pow_introd))
# 
# # clean country data
# datNat2 <- datNat %>%
#   mutate(locationType1 = map_chr(.x = locations, ~unlist(.x)[1]),
#          locations1 = map_chr(.x = locations, ~unlist(.x)[2]),
#          locationType2 = map_chr(.x = locations, ~unlist(.x)[3]),
#          locations2 = map_chr(.x = locations, ~unlist(.x)[4]),
#          nat_loc = case_when(locationType1 == "Native to:" ~ locations1,
#                              locationType2 == "Native to:" ~ locations2)) 
# 
# datIntro2 <- datIntro %>%
#   mutate(locations1 = map_chr(.x = locations, ~unlist(.x)[1]),
#          locations2 = map_chr(.x = locations, ~unlist(.x)[2]),
#          locations3 = map_chr(.x = locations, ~unlist(.x)[3]),
#          locations4 = map_chr(.x = locations, ~unlist(.x)[4]),
#          locations5 = map_chr(.x = locations, ~unlist(.x)[5]),
#          locations6 = map_chr(.x = locations, ~unlist(.x)[6]),
#          intro_loc = case_when(locations1 == "Introduced into:" ~ locations2,
#                                locations2 == "Introduced into:" ~ locations3, # for entries with "Literature" in locations1
#                                locations3 == "Introduced into:" ~ locations4,
#                                locations5 == "Introduced into:" ~ locations6)) 
# 
# # make sure all got locations
# datNat2 %>%
#   filter(is.na(nat_loc)) %>%
#   unnest(locations)
# # yes
# 
# datIntro2 %>%
#   filter(is.na(intro_loc)) %>%
#   select(Species)
# # Potamogeton natans and Potentilla verna have no introduced locations: add manually
# 
# # save raw data
# write_csv(datNat2 %>% select(Species, nat_loc), "intermediate-data/POW_native_range_scrape_20210811.csv")
# write_csv(datIntro2 %>% select(Species, intro_loc), "intermediate-data/POW_introd_range_scrape_20210811.csv")

datNat2 <- read_csv("intermediate-data/POW_native_range_scrape_20210811.csv")
datIntro2 <- read_csv("intermediate-data/POW_introd_range_scrape_20210811.csv")

# region names
world_regions <- map_data("world") %>%
  select(region) %>%
  unique()

# combine native and introduced datasets
# make long
# clean up location names
dat4 <- datNat2 %>%
  select(Species, nat_loc) %>%
  mutate(nat_loc = str_split(nat_loc, ", ")) %>%
  unnest(cols = nat_loc) %>%
  pivot_longer(cols = "nat_loc",
               names_to = "country_type",
               values_to = "country") %>%
  full_join(datIntro2 %>%
              filter(!is.na(intro_loc)) %>%
              select(Species, intro_loc) %>%
              mutate(intro_loc = str_split(intro_loc, ", ")) %>%
              unnest(cols = intro_loc) %>%
              pivot_longer(cols = "intro_loc",
                           names_to = "country_type",
                           values_to = "country")) %>%
  mutate(country_type = fct_recode(country_type, native = "nat_loc", introduced = "intro_loc"),
         country = case_when(country %in% str_replace(state.name, " ", "") ~ "USA",
                             country %in% state.name ~ "USA",
                             TRUE ~ country),
         country = str_replace_all(country, c("West-Central|Central|EastEuropean|East|Northeast|Northwest|North-Central|North|Southeast|Southwest|South-Central|South|Western|West|-|Inner"), ""),
         country = str_replace_all(country, c("Costa" = "Costa ", "New" = "New ", "Saudi" = "Saudi ", "Sri" = "Sri ")),
         country = case_when(country %in% c("RhodeI.", "AleutianIs.", "DistrictofColumbia") ~ "USA", # islands between Alaska and Russia, most are part of Alaska
                            country %in% c("EuropeanRus", "EuropeanRussi", "EuropeanR", "EuropeanRussi", "Buryatiya", "Irkutsk", "Kamchatka", "Khabarovsk", "Krasnoyarsk", "KurilIs.", "Magadan", "Primorye", "Sakhalin", "Siberia", "Tuva", "Yakutskiya", "Chita") ~ "Russia", # Kuril Islands disputed (Japan)
                            country %in% c("Alberta", "BritishColumbia", "Labrador", "Manitoba", "NorthwestTerritorie", "NovaScotia", "Ontario", "New Brunswick", "New foundland", "Nunavut", "PrinceEdwardI.", "Québec", "Saskatchewan", "Territorie", "Yukon") ~ "Canada",
                            country %in% c("NewSouthWales", "Victoria", "MacquarieIs.", "New Wales", "Queensland", "Tasmania", "ernTerritory") ~ "Australia",
                            country %in% c("Turkey-in-Europe", "TurkeyinEurope") ~ "Turkey",
                            country %in% c("AegeanIs.", "Kriti") ~ "Greece", # close to Greece and many (not all) are part of Greece
                            country %in% c("Altay", "Amur", "Himalaya", "Hainan", "Manchuria", "Qinghai", "Xinjiang", "Tibet")  ~ "China", # Amur river in China and Russia, all spp already have Russia
                            country %in% c("AndamanIs.", "Assam", "NicobarIs.") ~ "India",
                            country %in% c("AntipodeanIs.", "ChathamIs.", "KermadecIs.") ~ "New Zealand",
                            country == "Baleares" ~ "Spain",
                            country == "CanaryIs." ~ "Canary Islands",
                            country %in% c("CapeProvinces", "ernProvinces", "FreeState", "KwaZuluNatal", "MarionPrinceEdward") ~ "South Africa",
                            country == "CapeVerde" ~ "Cape Verde",
                            country == "Zaïre" ~ "Democratic Republic of the Congo",
                            country == "Congo" ~ "Republic of Congo",
                            country == "Corse" ~ "France",
                            country == "Føroyar" ~ "Faroe Islands",
                            country == "GreatBritain" ~ "UK",
                            country %in% c("Jawa", "LesserSundaIs.", "Maluku", "Sulawesi", "Sumatera") ~ "Indonesia",
                            country == "New Guinea" ~ "Papua New Guinea",
                            country == "Kirgizstan" ~ "Kyrgyzstan",
                            country == "Krym" ~ "Ukraine",
                            country == "LebanonSyria" ~ "Syria",
                            country %in% c("Madeira", "Selvagens") ~ "Portugal",
                            country == "Malaya" ~ "Malaysia",
                            country %in% c("Nanseishoto", "Ogasawarashoto") ~ "Japan",
                            country == "NorfolkIs." ~ "Norfolk Island",
                            country == "GulfofGuineaIs." ~ "Equatorial Guinea", # close mainland
                            country == "Panamá" ~ "Panama",
                            country %in% c("Sardegna", "Sicilia") ~ "Italy",
                            country == "Sinai" ~ "Egypt",
                            country %in% c("MexicoGulf", "MexicanPacificIs.") ~ "Mexico",
                            country == "Svalbard" ~ "Norway",
                            country == "Tadzhikistan" ~ "Tajikistan",
                            country == "Transcaucasus" ~ "Caucasus",
                            country == "Ascension" ~ "Ascension Island",
                            country == "Borneo" ~ "Indonesia", # part of 3 countries and Indonesia is the largest
                            country %in% c("CarolineIs.", "CookIs.") ~ "Kiribati",
                            country %in% c("CrozetIs.", "AmsterdamSt.PaulIs", "Kerguelen") ~ "French Southern and Antarctic Lands",
                            country %in% c("DesventuradosIs.", "erIs.", "JuanFernándezIs.") ~ "Chile",
                            country == "DominicanRepublic" ~ "Dominican Republic",
                            country == "ElSalvador" ~ "El Salvador",
                            country == "FalklandIs." ~ "Falkland Islands",
                            country == "IvoryCoast" ~ "Ivory Coast",
                            country %in% c("Marquesas", "SocietyIs.", "TubuaiIs.") ~ "French Polynesia",
                            country == "Réunion" ~ "Reunion",
                            country == "St.Helena" ~ "Saint Helena",
                            TRUE ~ country)) %>%
  unique() %>%
  full_join(tibble(country = c("Botswana", "Lesotho", "Namibia", "Reunion", "Saint Helena", "South Africa", "French Southern and Antarctic Lands", "Australia", "French Polynesia", "New Caledonia", "New Zealand", "Norfolk Island", "Argentina", "Brazil", "Chile", "Falkland Islands", "Paraguay", "Uruguay")) %>%
              mutate(Species = "Campylopus introflexus",
                     country_type = "native")) %>% # manual CABI entry (not in POW)
  full_join(tibble(country = c("Turkey", "Austria", "Belgium", "Czech Republic", "Denmark", "Faroe Islands", "France", "Germany", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Slovakia", "Spain", "Sweden", "Switzerland", "UK", "Canada", "USA")) %>%
              mutate(Species = "Campylopus introflexus",
                     country_type = "introduced")) %>% # manual CABI entry (not in POW)
  full_join(tibble(country = c("South Africa", "China")) %>%
              mutate(Species = "Potamogeton natans",
                     country_type = "introduced")) %>% # manual GCW/GBIF entry (not in POW)
  full_join(tibble(country = c("Japan", "USA", "Canada", "Russia")) %>%
              mutate(Species = "Potentilla verna",
                     country_type = "introduced")) %>% # manual GCW/GBIF entry (not in POW)
  full_join(tibble(country = c(rep("Caucasus", 3), 
                               rep("BalticStates", 3), 
                               rep("Korea", 2), 
                               rep("Yugoslavia", 7), 
                               rep("GulfStates", 6), 
                               rep("Sahara", 11), 
                               rep("LeewardIs.", 7),
                               rep("TrinidadTobago", 2),
                               rep("WindwardIs.", 4),
                               rep("Czechoslovakia", 2)),
                   country2 = c("Armenia", "Azerbaijan", "Georgia", 
                                "Estonia", "Latvia", "Lithuania", 
                                "North Korea", "South Korea", 
                                "Slovenia", "Croatia", "Bosnia and Herzegovina", "Montenegro", "Serbia", "Macedonia", "Kosovo",
                                "Bahrain", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "United Arab Emirates", 
                                "Algeria", "Chad", "Egypt", "Libya", "Mali", "Mauritania", "Morocco", "Niger", "Western Sahara", "Sudan", "Tunisia",
                                "Guadeloupe", "Antigua", "Barbuda", "Saint Kitts", "Nevis", "Saint Martin", "Virgin Islands",
                                "Trinidad", "Tobago",
                                "Dominica", "Saint Lucia", "Saint Vincent", "Grenada",
                                "Czech Republic", "Slovakia"))) %>%
  mutate(country = case_when(is.na(country2) ~ country,
                             !is.na(country2) ~ country2)) %>% # replace country group with countries
  filter(country != "TristandaCunha") # Tristan da Cunha not in country list

# summarize by location
dat5 <- dat4 %>%
  group_by(country_type, country) %>%
  count() %>%
  ungroup()


#### values for text ####

# top native continents
dat4 %>%
  filter(country_type == "native") %>%
  mutate(region = countrycode(sourcevar = country, origin = "country.name", destination = "region"),
         region = case_when(country %in% c("Ascension Island", "Azores", "Canary Islands", "French Southern and Antarctic Lands", "Reunion", "Saint Helena") ~ "islands",
                            country == "Western Sahara" ~ "Middle East & North Africa",
                            TRUE ~ region)) %>%
  select(Species, region) %>%
  unique() %>%
  group_by(region) %>%
  summarise(taxa = n_distinct(Species)) %>%
  ungroup() %>%
  mutate(perc = taxa/n_distinct(dat4$Species)) %>%
  arrange(desc(taxa))
# warning message is addressed

# native to US
dat4 %>%
  filter(country == "USA" & country_type == "native") %>%
  select(Species) %>%
  arrange(Species)
# according to USDA, Matricaria discoidea, Gnaphalium uliginosum, and Poa nemoralis are introduced

# top introduced continents
dat4 %>%
  filter(country_type == "introduced") %>%
  mutate(region = countrycode(sourcevar = country, origin = "country.name", destination = "region"),
         region = case_when(country %in% c("Ascension Island", "Azores", "Barbuda", "Canary Islands", "French Southern and Antarctic Lands", "Reunion", "Saint Helena") ~ "islands",
                            country %in% c("Barbuda", "Saint Martin", "Virgin Islands") ~ "Latin America & Caribbean",
                            TRUE ~ region)) %>%
  select(Species, region) %>%
  unique() %>%
  group_by(region) %>%
  summarise(taxa = n_distinct(Species)) %>%
  ungroup() %>%
  mutate(perc = taxa/n_distinct(dat4$Species)) %>%
  arrange(desc(taxa))
# warning message is addressed


#### map ####

# world map
world_map <- map_data("world") %>%
  mutate(country = region) %>%
  expand_grid(tibble(country_type = c("native", "introduced"))) %>%
  left_join(dat5) %>%
  mutate(n = replace_na(n, 0))

# missing data?
filter(dat5, !(country %in% world_map$country)) %>%
  data.frame()

# figure settings
title_size = 10
text_size = 8
fig_theme <- theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = text_size),
        legend.title = element_text(size = text_size,),
        strip.text = element_text(size = 14, hjust = 0, face = "bold"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(-8, "pt"),
        legend.position = c(0.1, 0.18),
        legend.background = element_blank(),
        plot.margin = margin(-4, -5, -5, -5, unit = "pt"))

# facet_labels
facet_labs <- tibble(facet_type = c("A", "B"),
                     range_type = c("Native ranges", "Introduced ranges"),
                     group = c(1, 1))

# figure
map_fig <- world_map %>% 
  filter(country != "Antarctica") %>%
  mutate(facet_type = fct_recode(country_type,
                                 "A" = "native",
                                 "B" = "introduced")) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n, color = n), size = 0.1) + 
  geom_polygon(data = map_data("state", region = "florida"), fill = "red", color = "red", size = 0.2) +
  facet_wrap(~ facet_type, ncol = 1) +
  geom_text(data = facet_labs, aes(label = range_type), x = 0.5, y = 100, check_overlap = T, size = 3.5) +
  scale_fill_distiller(direction = 1, palette = "PuBu", name = "Number\nof taxa") + 
  scale_color_distiller(direction = 1, palette = "PuBu", name = "Number\nof taxa") +
  coord_fixed(ylim = c(-60, 97)) + 
  fig_theme +
  guides(fill = guide_colorbar(barheight = 2.5, barwidth = 1), color = guide_colorbar(barheight = 2.5, barwidth = 1))

# figure
map_fig

