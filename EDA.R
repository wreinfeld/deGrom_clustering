### Walker Reinfeld
### STAT 301-1
### Final Project
### Preliminary EDA 

## loading packages
library(tidyverse)
library(ggstance)



## looking at unique pitch types in data set
unique(complete_data$pitch_name)


# Pitch Propotions --------------------------------------------------------

## proportions of pitches thrown by season and season type in career
complete_data %>% 
  group_by(season_year, season_type) %>% 
  ## obtaining pitches by year and season type
  mutate(total_pitches = n()) %>%
  ungroup() %>% 
  ## gouping by season_year, season_type and pitch_type
  group_by(season_year, season_type, pitch_type) %>% 
  mutate(pitches_per_type = n()) %>%
  mutate(pitch_prop = pitches_per_type / total_pitches) %>% 
  select(season_year, season_type, pitch_name, pitch_type, pitch_prop) %>%
  filter(season_type == "Regular") %>%
  ## removing NA and Intentional balls
  filter(!is.na(pitch_name) & !(pitch_type == "IN")) %>% 
  unique() %>% 
  ggplot(aes(x = as.numeric(season_year), y = pitch_prop, 
             color = fct_reorder2(pitch_name, season_year, pitch_prop))) +
  geom_line() +
  geom_point() +
  labs(color = "Pitch Name", x = "Year", title = "Regular Season Pitch Mix",
       y = "Proportion of Total Pitches") +
  theme(plot.title = element_text(hjust = 0.5))



# Pitch Result Proportions ------------------------------------------------



complete_data %>% 
  group_by(season_year, season_type) %>% 
  ## obtaining pitches by year and season type
  mutate(total_pitches = n()) %>%
  ungroup() %>% 
  group_by(season_year, season_type, type) %>% 
  mutate(type_prop = n() / total_pitches) %>% 
  select(season_year, season_type, pitch_type, pitch_name, type, type_prop) %>% 
  filter(season_type == "Regular") %>% 
  ## removing NA and intentional balls
  filter(!is.na(pitch_name) & !(pitch_type == "IN")) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(type = case_when(type == "S" ~ "Strike",
                          type == "B" ~ "Ball",
                          TRUE ~ "In play") %>% as.factor()) %>% 
  ggplot(aes(x = as.numeric(season_year), y = type_prop, 
             color = fct_reorder2(type, season_year, type_prop))) +
  geom_line() +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Ball, Strike, In Play", x = "Year", y = "Type Proportion", 
       Title = "Pitch Result Proportions")



## proportion of strikes balls and balls hit in play 
complete_data %>% 
  group_by(season_year, season_type, pitch_name) %>% 
  ## obtaining pitches by year and season type
  mutate(total_pitches = n()) %>%
  ungroup() %>% 
  group_by(season_year, pitch_name, season_type, type) %>% 
  mutate(type_prop = n() / total_pitches) %>% 
  select(season_year, season_type, pitch_type, pitch_name, type, type_prop) %>% 
  filter(season_type == "Regular") %>% 
  ## removing NA and intentional balls
  filter(!is.na(pitch_name) & !(pitch_type == "IN")) %>% 
  unique() %>%
  ungroup() %>% 
  mutate(type = case_when(type == "S" ~ "Strike",
                          type == "B" ~ "Ball",
                          TRUE ~ "In play") %>% as.factor()) %>% 
  ggplot(aes(x = as.numeric(season_year), y = type_prop, 
             color = fct_reorder2(type, season_year, type_prop))) +
  geom_line() +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~pitch_name) + 
  labs(color = "Ball, Strike, In Play", x = "Year", y = "Type Proportion", 
                                 Title = "Pitch Result Proportions")



# Strike out pitches ------------------------------------------------------

complete_data %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year, season_type) %>% 
  mutate(total_strikeout_pitches = n()) %>% 
  ungroup() %>% 
  group_by(season_year, season_type, pitch_type) %>% 
  mutate(so_by_pitch = n(),
         strike_out_prop = so_by_pitch / total_strikeout_pitches) %>%
  ungroup() %>% 
  select(season_year, season_type, pitch_name, 
         pitch_type, total_strikeout_pitches, so_by_pitch, strike_out_prop) %>%
  filter(season_type == "Regular") %>%
  ## removing NA and Intentional balls
  filter(!is.na(pitch_name) & !(pitch_type == "IN")) %>% 
  unique() %>%
  ggplot(aes(x = as.numeric(season_year), y = strike_out_prop, 
             color = fct_reorder2(pitch_name, season_year, strike_out_prop))) +
  geom_line() +
  geom_point() +
  labs(color = "Pitch Name", x = "Year", title = "Strike Out Pitch Mix") +
  theme(plot.title = element_text(hjust = 0.5))


complete_data %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year, season_type) %>% 
  mutate(total_nonstrikeout_pitches = n()) %>% 
  ungroup() %>% 
  group_by(season_year, season_type, pitch_type) %>% 
  mutate(pitch_by_type = n(),
         nonstrikeout_prop = pitch_by_type / total_nonstrikeout_pitches) %>%
  ungroup() %>% 
  select(season_year, season_type, pitch_name, 
         pitch_type, total_nonstrikeout_pitches, pitch_by_type, nonstrikeout_prop) %>%
  filter(season_type == "Regular") %>%
  ## removing NA and Intentional balls
  filter(!is.na(pitch_name) & !(pitch_type == "IN")) %>% 
  unique() %>%
  ggplot(aes(x = as.numeric(season_year), y = nonstrikeout_prop, 
             color = fct_reorder2(pitch_name, season_year, nonstrikeout_prop))) +
  geom_line() +
  geom_point() +
  labs(color = "Pitch Name", x = "Year", title = "Non-Strike Out Pitch Mix") +
  theme(plot.title = element_text(hjust = 0.5))



# Strike Out vs Non-Strike Out Speeds -------------------------------------
## taking a look at all pitches over the year with a facet
complete_data %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = season_year, y = release_speed, color = pitch_name)) +
  geom_boxplot() +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5))


pxp_reg_2015 %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2015 Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))

pxp_reg_2015 %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2015 non-Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))

### 2016

pxp_reg_2016 %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2016 Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))

pxp_reg_2016 %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2016 non-Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))


### 2017
pxp_reg_2017 %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2017 Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))

pxp_reg_2017 %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2017 non-Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))



### 2018 

pxp_reg_2018 %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2018 Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))

pxp_reg_2018 %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2018 non-Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))



pxp_reg_2018 %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular") %>% 
  ggplot(aes(x = release_speed, fill = pitch_name)) +
  geom_density(alpha = 0.3) +
  labs(title = "2018 non-Strikeout speeds by pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(80,100))


# Speed vs Spin Rate ------------------------------------------------------

complete_data %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_speed) & season_type == "Regular" &
           !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = release_speed, y = release_spin_rate, color = pitch_name)) +
  geom_point(alpha = 0.3) + 
  facet_wrap(~season_year)



# Strike Out vs Non-Strike Out Spin Rates -------------------------------------
complete_data %>% 
  filter(str_detect(events, "strikeout")) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_spin_rate) & season_type == "Regular") %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = pitch_name)) +
  geom_boxplot() +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Strikeout spin rates by pitch") +
  theme(plot.title = element_text(hjust = 0.5))


complete_data %>% 
  filter(!(str_detect(events, "strikeout"))) %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_spin_rate) & season_type == "Regular") %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = pitch_name)) +
  geom_boxplot() +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "non-Strikeout spin rates by pitch") +
  theme(plot.title = element_text(hjust = 0.5))


# Spin Rate  --------------------------------------------------------------


## taking a look at all pitches over the year with a facet
complete_data %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !is.na(release_spin_rate) & season_type == "Regular") %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = pitch_name)) +
  geom_boxplot() +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Spin Rates by Pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))


### taking a look at distribution of rpm of 4 seam fastballs across all years 
complete_data %>% 
  group_by(season_year) %>% 
  filter(pitch_type == "FF" & !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = season_type)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "4-Seam Fastball Spin Rates by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))

## spin rate increased in 2017 and 2018, post 2016 surgery to reset nerve in throwing elbow



### taking a look at distribution of rpm of 2 seam fastballs across all years 
complete_data %>% 
  group_by(season_year) %>% 
  filter(pitch_type == "FT" & !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = season_type)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "2-Seam Fastball Spin Rates by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))



## slider
complete_data %>% 
  group_by(season_year) %>% 
  filter(pitch_type == "SL" & !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = season_type)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Slider Spin Rates by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))



## curveball
complete_data %>% 
  group_by(season_year) %>% 
  filter(pitch_type == "CU" & !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = season_type)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Curveball Spin Rates by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))


## changeup
complete_data %>% 
  group_by(season_year) %>% 
  filter(pitch_type == "CH" & !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = season_type)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Changeup Spin Rates by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))






# Speeds ------------------------------------------------------------------

## looking at effective speeds over all years
complete_data %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & season_type == "Regular" & 
           !(pitch_type == "IN") & !is.na(effective_speed)) %>%
  ggplot(aes(x = season_year, y = effective_speed, color = pitch_name)) +
  geom_boxplot() +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Effective Pitch Speed by Year") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(ylim = c(75,100))


## looking at effective speeds for 2015 regular season
pxp_reg_2015 %>% 
  filter(!is.na(pitch_type) & !is.na(effective_speed) & !(pitch_type == "IN")) %>% 
  ggplot(aes(x = pitch_name, y = effective_speed, color = pitch_name)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "2015 Effective Pitch Speeds") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(75,100))





## looking at effective speeds for 2016 regular season
pxp_reg_2016 %>% 
  filter(!is.na(pitch_type) & !is.na(effective_speed) & !(pitch_type == "IN")) %>% 
  ggplot(aes(x = pitch_name, y = effective_speed, color = pitch_name)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "2016 Effective Pitch Speeds") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(75,100))



## looking at effective speeds for 2017 regular season
pxp_reg_2017 %>% 
  filter(!is.na(pitch_type) & !is.na(effective_speed) & !(pitch_type == "IN")) %>% 
  ggplot(aes(x = pitch_name, y = effective_speed, color = pitch_name)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "2017 Effective Pitch Speeds") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(75,100))




## looking at effective speeds for 2018 regular season
pxp_reg_2018 %>% 
  filter(!is.na(pitch_type) & !is.na(effective_speed)) %>% 
  ggplot(aes(x = pitch_name, y = effective_speed, color = pitch_name)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "2018 Effective Pitch Speeds") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(75,100))





## looking at release speed
complete_data %>% 
  group_by(season_year) %>%
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & !is.na(effective_speed)) %>%
  ggplot(aes(x = season_year, y = release_speed, color = season_type)) +
  geom_boxplot() +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Release Speed by Year") +
  theme(plot.title = element_text(hjust = 0.5))



# Release Positions -------------------------------------------------------

complete_data %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>% 
  filter(season_type == "Regular") %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ season_year, nrow = 1) +
  labs(title = "Release Points: 2015-2018") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4))


## looking at late game release positions
complete_data %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>%
  filter(season_type == "Regular") %>% 
  ## later in game 
  filter(game_pitch_num >= 75) %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ season_year, nrow = 1) +
  labs(title = "Release Point by Pitch: Late Game 2015-2018") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4))


## looking at early release positions
complete_data %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>%
  filter(season_type == "Regular") %>% 
  ## earlier in game 
  filter(game_pitch_num < 75) %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ season_year, nrow = 1) +
  labs(title = "Release Point by Pitch: Early Game 2015-2018") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4))




## looking at 2015 release positions
pxp_reg_2015 %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>%
  filter(season_type == "Regular") %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Release Point by Pitch: 2015") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = .5)) +
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4)) 


## looking at 2016 release positions
pxp_reg_2016 %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Release Point by Pitch: 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = .5)) +
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4)) 


## looking at 2017 release positions
pxp_reg_2017 %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Release Point by Pitch: 2017") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = .5)) +
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4)) 


## looking at 2018 release positions
pxp_reg_2018 %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") & 
           !(is.na(release_pos_x)) & !(is.na(release_pos_z))) %>% 
  ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name)) +
  geom_point(alpha = .2) +
  facet_wrap(~ pitch_name, nrow = 2) +
  labs(title = "Release Point by Pitch: 2018") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = .5)) +
  coord_cartesian(xlim = c(-2.5, -0.5), ylim = c(4.8, 6.4)) 



# Release Extensions ------------------------------------------------------

complete_data %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") &
           !is.na(release_extension) & season_type == "Regular") %>% 
  ggplot(aes(x = season_year, y = release_extension, color = pitch_name)) +
  geom_boxplot() +
  facet_wrap(~pitch_name)


## release extensions 2015
pxp_reg_2015 %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") &
           !is.na(release_extension)) %>% 
  ggplot(aes(x = pitch_name, y = release_extension, color = pitch_name)) +
  geom_boxplot(varwidth = TRUE) +
  coord_cartesian(ylim = c(5.5,8.0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## release extensions 2018
pxp_reg_2018 %>% 
  filter(!is.na(pitch_type) & !(pitch_type == "IN") &
           !is.na(release_extension)) %>% 
  ggplot(aes(x = pitch_name, y = release_extension, color = pitch_name)) +
  geom_boxplot(varwidth = TRUE) +
  coord_cartesian(ylim = c(5.5,8.0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






