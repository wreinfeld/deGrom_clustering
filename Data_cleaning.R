
# Load Packages -----------------------------------------------------------

## loading packages
library(tidyverse)
library(janitor)
library(lubridate)


# Loading data ------------------------------------------------------------

## reading in 2015 pitch by pitch
pxp_reg_2015 <- read_csv(file = "Data/Unprocessed/deGrom_2015_pxp_regular.csv", 
                         na = c("null", "NA", NA, "")) %>% 
  clean_names() %>% 
  arrange(-row_number()) %>% 
  group_by(game_date) %>% 
  mutate(game_pitch_num = row_number()) %>% 
  ungroup() %>% 
  mutate(season_year = "2015",
         season_type = "Regular",
         runner_on = ifelse(!(is.na(on_1b)) | !(is.na(on_2b)) | !(is.na(on_3b)), 1, 0),
         scoring_position = ifelse(!(is.na(on_2b)) | !(is.na(on_3b)), 1, 0)) %>% 
  select(-pitcher_1, -fielder_2_1)

write_csv(pxp_reg_2015, "data/processed/2015_reg_all.csv")

## reading in 2015 pitch by pitch
pxp_post_2015 <- read_csv(file = "Data/Unprocessed/deGrom_2015_pxp_Postseason.csv", 
                          na = c("null", "NA", NA, "")) %>% 
  clean_names() %>% 
  arrange(-row_number()) %>% 
  group_by(game_date) %>% 
  mutate(game_pitch_num = row_number()) %>% 
  ungroup() %>% 
  mutate(season_year = "2015",
         season_type = "Postseason",
         runner_on = ifelse(!(is.na(on_1b)) | !(is.na(on_2b)) | !(is.na(on_3b)), 1, 0),
         scoring_position = ifelse(!(is.na(on_2b)) | !(is.na(on_3b)), 1, 0)) %>% 
  ## these columns are duplicates can be confirmation showed below
  select(-pitcher_1, -fielder_2_1)

write_csv(pxp_post_2015, "data/processed/2015_post_all.csv")

## reading in 2016 pitch by pitch
pxp_reg_2016 <- read_csv(file = "Data/Unprocessed/deGrom_2016_pxp.csv", 
                         na = c("null", "NA", NA, "")) %>% 
  clean_names() %>% 
  arrange(-row_number()) %>% 
  group_by(game_date) %>% 
  mutate(game_pitch_num = row_number()) %>% 
  ungroup() %>% 
  mutate(season_year = "2016",
         season_type = "Regular",
         runner_on = ifelse(!(is.na(on_1b)) | !(is.na(on_2b)) | !(is.na(on_3b)), 1, 0),
         scoring_position = ifelse(!(is.na(on_2b)) | !(is.na(on_3b)), 1, 0)) %>% 
  ## these columns are duplicates can be confirmation showed below
  select(-pitcher_1, -fielder_2_1)


write_csv(pxp_reg_2016, "data/processed/2016_all.csv")

## reading in 2017 pitch by pitch
pxp_reg_2017 <- read_csv(file = "Data/Unprocessed/deGrom_2017_pxp.csv", 
                         na = c("null", "NA", NA, "")) %>% 
  clean_names() %>% 
  arrange(-row_number()) %>% 
  group_by(game_date) %>% 
  mutate(game_pitch_num = row_number()) %>% 
  ungroup() %>% 
  mutate(season_year = "2017",
         season_type = "Regular",
         runner_on = ifelse(!(is.na(on_1b)) | !(is.na(on_2b)) | !(is.na(on_3b)), 1, 0),
         scoring_position = ifelse(!(is.na(on_2b)) | !(is.na(on_3b)), 1, 0)) %>% 
  ## these columns are duplicates can be confirmation showed below
  select(-pitcher_1, -fielder_2_1)

write_csv(pxp_reg_2017, "data/processed/2017_all.csv")

## reading in 2018 pitch by pitch
pxp_reg_2018 <- read_csv(file = "Data/Unprocessed/deGrom_2018_pxp.csv", 
                         na = c("null", "NA", NA, "")) %>% 
  clean_names() %>% 
  arrange(-row_number()) %>% 
  group_by(game_date) %>% 
  mutate(game_pitch_num = row_number()) %>% 
  ungroup() %>% 
  mutate(season_year = "2018",
         season_type = "Regular",
         runner_on = ifelse(!(is.na(on_1b)) | !(is.na(on_2b)) | !(is.na(on_3b)), 1, 0),
         scoring_position = ifelse(!(is.na(on_2b)) | !(is.na(on_3b)), 1, 0)) %>% 
  ## these columns are duplicates can be confirmation showed below
  select(-pitcher_1, -fielder_2_1)


write_csv(pxp_reg_2018, "data/processed/2018_all.csv")


# Combine_with_all variables ----------------------------------------------

complete_data <- rbind(pxp_reg_2015, pxp_post_2015, pxp_reg_2016, 
                       pxp_reg_2017, pxp_reg_2018)


saveRDS(complete_data, "data/processed/all_variables.rds")



# Variable Selection ------------------------------------------------------

