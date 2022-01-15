# fl-plants-horizon-scan
Code and data for horizon scan of invasive plant threats to Florida. This repository accompanies the manuscript "Scanning the horizon for invasive plant threats to Florida, USA" by Amy E. Kendig, Susan Canavan, Patti J. Anderson, S. Luke Flory, Lyn A. Gettys, Doria R. Gordon, Basil V. Iannone III, John M. Kunzer, Tabitha Petri, Ian A. Pfingsten, and Deah Lieurance.

### Contents
- code: descriptions below 
- data: descriptions below  
- intermediate-data: datasets produced by processing original data (see code) or produced through horizon scan process (descriptions below)  
- fl-plants-horizon-scan.Rproj: RStudio project for running R scripts

|code                             |description |
|:--------------------------------|:---------------------------------------------------------------------------------------------------------------|
|gcw_processing.R                 |R script to format data downloaded from the Global Compendium of Weeds |
|native_introduced_ranges.R       |R script to create map of native and introduced ranges of taxa on the final list |
|random_draws_plant_families.R    |R script to evaluate over- and underrepresentation of plant families in initial and final list |
|review_process_comparison.R      |R script to evaluate differences in scores before and after peer-review and consensus-building |
|scores_certainty_pathways.R      |R script to create figures of scores, certainty, and pathways for final list |
|risk_scores_analys.R             |R script to evaluate final risk scores |
|pathways_process.R               |R script to process pathways to introduction data |
|taxa_list_processing.R           |R script to create list used for rapid risk assessments from an initial list |
|taxonomic_method_comparison.R    |R script to check consistency of taxa lists produced by original (initial commit) and revised taxonomic methods |

|data                                                       |description |
|:----------------------------------------------------------|:---------------------------------------------------------------|
|cab_list_full.csv                                          |list of potential invasive species to Florida generated by CABI Horizon Scan Tool on November 15, 2019 |
|GCW_full_list_020420.csv                                   |Global Compendium of Weeds downloaded on February 4, 2020 |
|PlantAtlasDataExport-20191211-194219.csv                   |Atlas of Florida plants downloaded December 11, 2019 |
|Taxon_x_List_GloNAF_vanKleunenetal2018Ecology_121119.csv   |GloNAF 1.2 database downloaded December 11, 2019  |
|the-plant-list                                             |The Plant List Database downloaded August 3, 2021 |

|intermediate-data                           |description |
|:-------------------------------------------|:------------------------------------------------------------------------------|
|federal_noxious_weed_list.csv               |manually formatted version of the USDA Federal Noxious Weed List downloaded March 16, 2020 |
|first_round_assessments_050120.csv          |rapid risk assessments for horizon scan pre-peer-review |
|fl_prohibited_plants.csv                    |manually compiled list of prohibited plants in Florida based on the Florida Noxious Weed List, Florida Prohibited Plants list, and Florida Invasive Species Council (all downloaded March 9, 2020)  |
|horizon_scan_plants_full_reviews_080321     |rapid risk assessments for horizon scan post-peer-review and consensus-building |


## Comments
References for data can be found in manuscript (link will be added when preprint available) and details on data acquisition can be found in the R script taxa_list_processing.R.
