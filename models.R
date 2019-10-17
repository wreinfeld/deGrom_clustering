library(mltools)
library(data.table)
library(knitr)
library(gridExtra)
library(ggdendro)

source("Data_cleaning.R")


## curveball
complete_data %>% 
  group_by(season_year) %>% 
  filter(pitch_type == "CU" & !is.na(release_spin_rate)) %>% 
  ggplot(aes(x = season_year, y = release_spin_rate, color = season_type)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Curveball Spin Rates by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(500,3500))


model_dat_full <- complete_data %>% 
  filter(pitch_type != "IN" & !(is.na(pitch_type))) %>% 
  filter(balls != 4) %>%
  filter(season_type == "Regular") %>% 
  select(release_speed, 
         release_extension, release_pos_x,release_pos_z,
         release_spin_rate, type, description, stand, balls, strikes,
         game_year, inning, effective_speed,
         pitch_name, runner_on, scoring_position, events) %>% 
  mutate(game_year = as.factor(game_year),
         inning = as.factor(inning),
         runner_on = as.factor(runner_on),
         scoring_position = as.factor(scoring_position),
         balls = as.factor(balls),
         strikes = as.factor(strikes),
         type = as.factor(type),
         stand = as.factor(stand)) %>% 
  mutate(events = ifelse(is.na(events), "nothing", events)) %>% 
  drop_na()



complete_data %>% 
  filter(pitch_type == "FF" | pitch_type == "FT") %>% 
  filter(game_year == 2016) %>% 
  ggplot(aes(x = release_spin_rate, y = release_speed, color = pitch_type)) +
  geom_point(alpha = 0.5)




# One hot and scaling  ----------------------------------------------------


model_scaled_full <- model_dat_full %>% select(-pitch_name, -type, -description, -stand,
                     -game_year, -balls, -strikes, -inning, -runner_on, -scoring_position, -events) %>% 
  scale() %>% as_tibble() %>% 
  bind_cols(model_dat_full %>% 
              select(type, stand,
                     game_year, balls, strikes, inning, runner_on, scoring_position)) %>% 
  as.data.table() %>% 
  mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat_full %>% 
                                                     select(description, pitch_name, events))




# Helper Functions --------------------------------------------------------

get_cluster <- function(x, clust_obj){
  
  if(class(clust_obj) == "kmeans"){
    clust = clust_obj$cluster
  } else {
    clust = clust_obj
  }
  
  out = x %>% 
    mutate(cluster = clust)
  
  return(out)
}

# Plot data with cluster labels
plot_cluster <- function(cluster_dat, title){
  
  plt = ggplot(cluster_dat) + 
    geom_point(aes(release_spin_rate, release_speed, color = factor(cluster)),
               alpha = 0.3) + 
    coord_cartesian(xlim = c(-6,4))
  
  return(plt)
}

# Add labels to plots
label_plot <- function(plot, title){
  
  return(plot + labs(title = title))
}
# Extract within-cluster SS from K-means object
get_within_ss <- function(kmean_obj){
  return(kmean_obj$tot.withinss)
}

run_hclust <- function(x, meth){
  return(hclust(dist(x), method = meth))
}

cut_hclust <- function(hclust_obj, ncuts){
  return(cutree(hclust_obj, ncuts))
}

# K-Means -----------------------------------------------------------------

set.seed(11)

dat_kmeans_full <-  tibble(xmat = list(model_scaled_full %>% select(-description, -pitch_name, -events))) %>%
  crossing(nclust = 3:10)

dat_kmeans_full <-  dat_kmeans_full %>% 
  mutate(kmean = map2(xmat, nclust, kmeans, nstart=20, iter.max = 20), # Fit K-means
         within_ss = map(kmean, get_within_ss), # Get within-cluster SS
         clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
         plots = map(clusters, plot_cluster)) # Plot clusters


kmeans_dat_full3 <- dat_kmeans_full %>% 
  pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)

kmeans_dat_full3 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))

kmeans_dat_full4 <- dat_kmeans_full %>% 
  pluck("clusters", 2) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)

kmeans_dat_full4 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))




kmeans_dat_full5 <- dat_kmeans_full %>% 
  pluck("clusters", 3) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)

kmeans_dat_full5 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))



kmeans_dat_full6 <- dat_kmeans_full %>% 
  pluck("clusters", 4) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)


kmeans_dat_full6 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))

kmeans_dat_full7 <- dat_kmeans_full %>% 
  pluck("clusters", 5) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)


kmeans_dat_full7 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))

kmeans_dat_full8 <- dat_kmeans_full %>% 
  pluck("clusters", 6) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)

kmeans_dat_full8 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))


kmeans_dat_full9 <- dat_kmeans_full %>% 
  pluck("clusters", 7) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)

kmeans_dat_full9 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))

kmeans_dat_full10 <- dat_kmeans_full %>% 
  pluck("clusters", 8) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat_full)


kmeans_dat_full10 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))


dat_kmeans_full %>% select(nclust, within_ss) %>% 
  kable(digits = 3)

wss_full <- dat_kmeans_full %>%
  select(nclust, within_ss) %>%
  unnest(within_ss)


centers_full <- dat_kmeans_full %>% 
  pluck("kmean", 3) %>% pluck("centers")


### looking at differences between 4-seam and 2-seam
fourseam_full <- centers_full[5,]
twoseam_full <- centers_full[2,]
slider_full <- centers_full[1,]
changeup_full <- centers_full[4,]
curveball_full <- centers_full[3,]



## difference between 4_seam and 2_seam -- release positions
abs(fourseam_full - twoseam_full)


ggplot(wss_full) + 
  geom_line(aes(nclust, within_ss))

grid.arrange(grobs = dat_kmeans_full$plots)


# 2018 Clusters -----------------------------------------------------------

model_dat2018 <- model_dat_full %>% 
  filter(game_year == 2018) 


model_scaled2018 <- model_dat2018 %>% 
  select(-pitch_name, -type, -description, 
         -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position,
         -events) %>% 
  scale() %>% as_tibble() %>% 
  bind_cols(model_dat2018 %>% 
              select(type, stand,
                     balls, strikes, inning, runner_on, scoring_position)) %>% 
  as.data.table() %>% 
  mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat2018 %>% select(description, pitch_name,
                                                                            events))


dat_kmeans2018 <-  tibble(xmat = list(model_scaled2018 %>% select(-description, -pitch_name,
                                                                  -events))) %>%
  crossing(nclust = 5)

dat_kmeans2018 <-  dat_kmeans2018 %>% 
  mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
         within_ss = map(kmean, get_within_ss), # Get within-cluster SS
         clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
         plots = map(clusters, plot_cluster)) # Plot clusters


grid.arrange(grobs = dat_kmeans2018$plots)

kmeans_dat2018_5 <- dat_kmeans2018 %>% 
  pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat2018)

kmeans_dat2018_5 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))


centers_2018 <- dat_kmeans2018 %>% 
  pluck("kmean", 1) %>% pluck("centers")
  

fourseam_2018 <- centers_2018[1,]
twoseam_2018 <- centers_2018[3,]
slider_2018 <- centers_2018[4,]
curveball_2018 <- centers_2018[2,]
changeup_2018 <- centers_2018[5,]







# 2017 Clusters -----------------------------------------------------------

model_dat2017 <- model_dat_full %>% 
  filter(game_year == 2017) 


model_scaled2017 <- model_dat2017 %>% 
  select(-pitch_name, -type, -description, 
         -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position, -events) %>% 
  scale() %>% as_tibble() %>% 
  bind_cols(model_dat2017 %>% 
              select(type, stand,
                     balls, strikes, inning, runner_on, scoring_position)) %>% 
  as.data.table() %>% 
  mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat2017 %>% select(description, pitch_name,
                                                                            events))


dat_kmeans2017 <-  tibble(xmat = list(model_scaled2017 %>% select(-description, -pitch_name,
                                                                  -events))) %>%
  crossing(nclust = 5)

dat_kmeans2017 <-  dat_kmeans2017 %>% 
  mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
         within_ss = map(kmean, get_within_ss), # Get within-cluster SS
         clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
         plots = map(clusters, plot_cluster)) # Plot clusters


kmeans_dat2017_5 <- dat_kmeans2017 %>% 
  pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat2017)

kmeans_dat2017_5 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))


grid.arrange(grobs = dat_kmeans2017$plots)


centers_2017 <- dat_kmeans2017 %>% 
  pluck("kmean", 1) %>% pluck("centers")


fourseam_2017 <- centers_2017[3,]
twoseam_2017 <- centers_2017[5,]
curveball_2017 <- centers_2017[4,]
slider_2017 <- centers_2017[2,]
changeup_2017 <- centers_2018[1,]




# 2016 Clusters -----------------------------------------------------------

model_dat2016 <- model_dat_full %>% 
  filter(game_year == 2016) 


model_scaled2016 <- model_dat2016 %>% 
  select(-pitch_name, -type, -description, 
         -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position, -events) %>% 
  scale() %>% as_tibble() %>% 
  bind_cols(model_dat2016 %>% 
              select(type, stand,
                     balls, strikes, inning, runner_on, scoring_position)) %>% 
  as.data.table() %>% 
  mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat2016 %>% 
                                                     select(description, pitch_name, events))


dat_kmeans2016 <-  tibble(xmat = list(model_scaled2016 %>% 
                                        select(-description, -pitch_name, -events))) %>%
  crossing(nclust = 5)


grid.arrange(grobs = dat_kmeans2016$plots)

dat_kmeans2016 <-  dat_kmeans2016 %>% 
  mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
         within_ss = map(kmean, get_within_ss), # Get within-cluster SS
         clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
         plots = map(clusters, plot_cluster)) # Plot clusters


kmeans_dat2016_5 <- dat_kmeans2016 %>% 
  pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
  bind_cols(model_dat2016)

kmeans_dat2016_5 %>% 
  group_by(cluster) %>% 
  summarise(mean_spin = mean(release_spin_rate),
            mean_release_speed = mean(release_speed),
            mean_effective_speed = mean(effective_speed),
            mean_release_extension = mean(release_extension))



### 2016 clusters did not do a good job of seperatin pitches so cannot classify pitches well for 
### early differences


# Kmeans in Play ----------------------------------------------------------
 model_dat_in_play <- model_dat_full %>% 
   filter(type == "X") 
 
 
 model_scaled_in_play <- model_dat_in_play %>% 
   select(-pitch_name, -type, -description, 
          -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position,
          -events) %>% 
   scale() %>% as_tibble() %>% 
   bind_cols(model_dat_in_play %>% 
               select(stand,
                      game_year, balls, strikes, inning, runner_on, scoring_position)) %>% 
   as.data.table() %>% 
   mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat_in_play %>% select(description, 
                                                                                 pitch_name,
                                                                                 events))
 
 
 dat_kmeans_in_play <-  tibble(xmat = list(model_scaled_in_play %>% select(-description, -pitch_name,
                                                                           -events))) %>%
   crossing(nclust = 5)
 
 dat_kmeans_in_play <-  dat_kmeans_in_play %>% 
   mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
          within_ss = map(kmean, get_within_ss), # Get within-cluster SS
          clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
          plots = map(clusters, plot_cluster)) # Plot clusters
 
 
 kmeans_dat_in_play5 <- dat_kmeans_in_play %>% 
   pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
   bind_cols(model_dat_in_play)
 
 kmeans_dat_in_play5 %>% 
   group_by(cluster) %>% 
   summarise(mean_spin = mean(release_spin_rate),
             mean_release_speed = mean(release_speed),
             mean_effective_speed = mean(effective_speed),
             mean_release_extension = mean(release_extension))
 
 
 
 grid.arrange(grobs = dat_kmeans_in_play$plots)
 
 centers_in_play <- dat_kmeans_in_play %>% 
   pluck("kmean", 1) %>% pluck("centers")
 
 fourseam_in_play <- centers_in_play[2,]
 twoseam_in_play <- centers_in_play[3,]
 changeup_in_play <- centers_in_play[1,]
 curveball_in_play <- centers_in_play[4,]
 slider_in_play <- centers_in_play[5,]
 

# Kmean not in play -------------------------------------------------------

 model_dat_not_in_play <- model_dat_full %>% 
   filter(type != "X") 
 
 
 model_scaled_not_in_play <- model_dat_not_in_play %>% 
   select(-pitch_name, -type, -description, 
          -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position, -events) %>% 
   scale() %>% as_tibble() %>% 
   bind_cols(model_dat_not_in_play %>% 
               select(stand,
                      game_year, balls, strikes, inning, runner_on, scoring_position)) %>% 
   as.data.table() %>% 
   mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat_not_in_play %>% 
                                                      select(description, pitch_name,
                                                             events))
 
 
 dat_kmeans_not_in_play <-  tibble(xmat = list(model_scaled_not_in_play %>% 
                                                 select(-description, -pitch_name,
                                                        -events))) %>%
   crossing(nclust = 5)
 
 dat_kmeans_not_in_play <-  dat_kmeans_not_in_play %>% 
   mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
          within_ss = map(kmean, get_within_ss), # Get within-cluster SS
          clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
          plots = map(clusters, plot_cluster)) # Plot clusters
 
 
 kmeans_dat_not_in_play5 <- dat_kmeans_not_in_play %>% 
   pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
   bind_cols(model_dat_not_in_play)
 
 kmeans_dat_not_in_play5 %>% 
   group_by(cluster) %>% 
   summarise(mean_spin = mean(release_spin_rate),
             mean_release_speed = mean(release_speed),
             mean_effective_speed = mean(effective_speed),
             mean_release_extension = mean(release_extension))
 
 grid.arrange(grobs = dat_kmeans_not_in_play$plots) 
 
 
 centers_not_in_play <- dat_kmeans_not_in_play %>% 
   pluck("kmean", 1) %>% pluck("centers")
 
 fourseam_not_in_play <- centers_not_in_play[4,]
 twoseam_not_in_play <- centers_not_in_play[2,]
 changeup_not_in_play <- centers_not_in_play[3,]
 curveball_not_in_play <- centers_not_in_play[5,]
 slider_not_in_play <- centers_not_in_play[1,]
 
 

# Kmeans Strikeout --------------------------------------------------------

 model_dat_strikeout <- model_dat_full %>% 
   filter(str_detect(events, "strikeout")) 
 
 
 model_scaled_strikeout <- model_dat_strikeout %>% 
   select(-pitch_name, -type, -description, 
          -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position, -events) %>% 
   scale() %>% as_tibble() %>% 
   bind_cols(model_dat_strikeout %>% 
               select(stand,
                      game_year, balls, strikes, inning, runner_on, scoring_position)) %>% 
   as.data.table() %>% 
   mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat_strikeout %>% 
                                                      select(description, pitch_name,
                                                             events))
 
 
 dat_kmeans_strikeout <-  tibble(xmat = list(model_scaled_strikeout %>% 
                                                 select(-description, -pitch_name,
                                                        -events))) %>%
   crossing(nclust = 5)
 
 dat_kmeans_strikeout <-  dat_kmeans_strikeout %>% 
   mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
          within_ss = map(kmean, get_within_ss), # Get within-cluster SS
          clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
          plots = map(clusters, plot_cluster)) # Plot clusters
 
 
 kmeans_dat_strikeout5 <- dat_kmeans_strikeout %>% 
   pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
   bind_cols(model_dat_strikeout)
 
 kmeans_dat_strikeout5 %>% 
   group_by(cluster) %>% 
   summarise(mean_spin = mean(release_spin_rate),
             mean_release_speed = mean(release_speed),
             mean_effective_speed = mean(effective_speed),
             mean_release_extension = mean(release_extension))
 
 grid.arrange(grobs = dat_kmeans_strikeout$plots)  
 
 centers_strikeout <- dat_kmeans_strikeout %>% 
   pluck("kmean", 1) %>% pluck("centers")
 
 fourseam_strikeout <- centers_strikeout[1,]
 twoseam_strikeout <- centers_strikeout[2,]
 changeup_strikeout <- centers_strikeout[4,]
 curveball_strikeout <- centers_strikeout[3,]
 slider_strikeout <- centers_strikeout[5,]
 
 
 # Kmeans not_Strikeout --------------------------------------------------------
 
 model_dat_not_strikeout <- model_dat_full %>% 
   filter(!(str_detect(events, "strikeout"))) 
 
 
 model_scaled_not_strikeout <- model_dat_not_strikeout %>% 
   select(-pitch_name, -type, -description, 
          -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position, -events) %>% 
   scale() %>% as_tibble() %>% 
   bind_cols(model_dat_not_strikeout %>% 
               select(stand,
                      game_year, balls, strikes, inning, runner_on, scoring_position)) %>% 
   as.data.table() %>% 
   mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat_not_strikeout %>% 
                                                      select(description, pitch_name,
                                                             events))
 
 
 dat_kmeans_not_strikeout <-  tibble(xmat = list(model_scaled_not_strikeout %>% 
                                               select(-description, -pitch_name,
                                                      -events))) %>%
   crossing(nclust = 5)
 
 dat_kmeans_not_strikeout <-  dat_kmeans_not_strikeout %>% 
   mutate(kmean = map2(xmat, nclust, kmeans, nstart=20, iter.max = 20), # Fit K-means
          within_ss = map(kmean, get_within_ss), # Get within-cluster SS
          clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
          plots = map(clusters, plot_cluster)) # Plot clusters
 
 
 kmeans_dat_not_strikeout5 <- dat_kmeans_not_strikeout %>% 
   pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
   bind_cols(model_dat_not_strikeout)
 
 kmeans_dat_not_strikeout5 %>% 
   group_by(cluster) %>% 
   summarise(mean_spin = mean(release_spin_rate),
             mean_release_speed = mean(release_speed),
             mean_effective_speed = mean(effective_speed),
             mean_release_extension = mean(release_extension))
 
 grid.arrange(grobs = dat_kmeans_not_strikeout$plots)  
 
 
 centers_not_strikeout <- dat_kmeans_not_strikeout %>% 
   pluck("kmean", 1) %>% pluck("centers")
 
 fourseam_not_strikeout <- centers_not_strikeout[4,]
 twoseam_not_strikeout <- centers_not_strikeout[1,]
 changeup_not_strikeout <- centers_not_strikeout[5,]
 curveball_not_strikeout <- centers_not_strikeout[3,]
 slider_not_strikeout <- centers_not_strikeout[2,]
 
 

# comparisions ------------------------------------------------------------

fourseam_year_diff <- abs(fourseam_2018 - fourseam_2017)

twoseam_year_diff <- abs(twoseam_2018 - twoseam_2017)

changeup_year_diff <- abs(changeup_2018 - changeup_2017)

slider_year_diff <- abs(slider_2018 - slider_2017)

curveball_year_diff <- abs(curveball_2018 - curveball_2017)


fourseam_in_play_diff <- abs(fourseam_in_play - fourseam_not_in_play)

twoseam_in_play_diff <- abs(twoseam_in_play - twoseam_not_in_play)

changeup_in_play_diff <- abs(changeup_in_play - changeup_not_in_play)

slider_in_play_diff <- abs(slider_in_play - slider_not_in_play)

curveball_in_play_diff <- abs(curveball_in_play - curveball_not_in_play)




fourseam_strikeout_diff <- abs(fourseam_strikeout - fourseam_not_strikeout)

twoseam_strikeout_diff <- abs(twoseam_strikeout - twoseam_not_strikeout)

changeup_strikeout_diff <- abs(changeup_strikeout - changeup_not_strikeout)

curveball_strikeout_diff <- abs(curveball_strikeout - curveball_not_strikeout)

slider_strikeout_diff <- abs(slider_strikeout - slider_not_strikeout)
 


 # Using 2017 Cluster to Predict 2018 Membership ---------------------------
 # predict.kmeans <- function(object,
 #                            newdata,
 #                            method = c("centers", "classes")) {
 #   method <- match.arg(method)
 #   
 #   centers <- object$centers
 #   ss_by_center <- apply(centers, 1, function(x) {
 #     colSums((t(newdata) - x) ^ 2)
 #   })
 #   best_clusters <- apply(ss_by_center, 1, which.min)
 #   
 #   if (method == "centers") {
 #     centers[best_clusters, ]
 #   } else {
 #     best_clusters
 #   }
 # }
 # 
 # 
 # mod_2017 <- dat_kmeans2017 %>% pluck("kmean",1)
 # 
 # mod_2017$centers
 # 
 # predict.kmeans(mod_2017, model_scaled2018,
 #                method = "centers")
 
 
 # Kmeans 2016-2018 --------------------------------------------------------
 
 # model_dat2016_2018 <- model_dat_full %>% 
 #   filter(game_year != 2015) 
 # 
 # 
 # model_scaled2016_2018 <- model_dat2016_2018 %>% 
 #   select(-pitch_name, -type, -description, 
 #          -stand,-game_year, -balls, -strikes, -inning, -runner_on, -scoring_position) %>% 
 #   scale() %>% as_tibble() %>% 
 #   bind_cols(model_dat2016_2018 %>% 
 #               select(type, stand,
 #                      balls, strikes, inning, runner_on, scoring_position)) %>% 
 #   as.data.table() %>% 
 #   mltools::one_hot() %>% as_tibble() %>% bind_cols(model_dat2016_2018 %>% select(description, pitch_name))
 # 
 # 
 # dat_kmeans2016_2018 <-  tibble(xmat = list(model_scaled2016_2018 %>% select(-description, -pitch_name))) %>%
 #   crossing(nclust = 3:10)
 # 
 # dat_kmeans2016_2018 <-  dat_kmeans2016_2018 %>% 
 #   mutate(kmean = map2(xmat, nclust, kmeans, nstart=20, iter.max = 20), # Fit K-means
 #          within_ss = map(kmean, get_within_ss), # Get within-cluster SS
 #          clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
 #          plots = map(clusters, plot_cluster)) # Plot clusters
 # 
 # 
 # kmeans_dat2016_2018_5 <- dat_kmeans2016_2018 %>% 
 #   pluck("clusters", 1) %>% unnest() %>% select(cluster) %>% 
 #   bind_cols(model_dat2016_2018)
 # 
 # kmeans_dat2016_2018_5 %>% 
 #   group_by(cluster) %>% 
 #   summarise(mean_spin = mean(release_spin_rate),
 #             mean_release_speed = mean(release_speed),
 #             mean_effective_speed = mean(effective_speed),
 #             mean_release_extension = mean(release_extension))
 # 
 # kmeans_dat2016_2018_6 <- dat_kmeans2016_2018 %>% 
 #   pluck("clusters", 2) %>% unnest() %>% select(cluster) %>% 
 #   bind_cols(model_dat2016_2018)
 # 
 # 
 # kmeans_dat2016_2018_6 %>% 
 #   group_by(cluster) %>% 
 #   summarise(mean_spin = mean(release_spin_rate),
 #             mean_release_speed = mean(release_speed),
 #             mean_effective_speed = mean(effective_speed),
 #             mean_release_extension = mean(release_extension))
 # 
 # kmeans_dat2016_2018_7 <- dat_kmeans2016_2018 %>% 
 #   pluck("clusters", 3) %>% unnest() %>% select(cluster) %>% 
 #   bind_cols(model_dat2016_2018)
 # 
 # 
 # kmeans_dat2016_2018_7 %>% 
 #   group_by(cluster) %>% 
 #   summarise(mean_spin = mean(release_spin_rate),
 #             mean_release_speed = mean(release_speed),
 #             mean_effective_speed = mean(effective_speed),
 #             mean_release_extension = mean(release_extension))
 # 
 # kmeans_dat2016_2018_8 <- dat_kmeans2016_2018 %>% 
 #   pluck("clusters", 4) %>% unnest() %>% select(cluster) %>% 
 #   bind_cols(model_dat2016_2018)
 # 
 # kmeans_dat2016_2018_8 %>% 
 #   group_by(cluster) %>% 
 #   summarise(mean_spin = mean(release_spin_rate),
 #             mean_release_speed = mean(release_speed),
 #             mean_effective_speed = mean(effective_speed),
 #             mean_release_extension = mean(release_extension))
 # 
 # 
 # kmeans_dat2016_2018_9 <- dat_kmeans2016_2018 %>% 
 #   pluck("clusters", 5) %>% unnest() %>% select(cluster) %>% 
 #   bind_cols(model_dat2016_2018)
 # 
 # kmeans_dat2016_2018_9 %>% 
 #   group_by(cluster) %>% 
 #   summarise(mean_spin = mean(release_spin_rate),
 #             mean_release_speed = mean(release_speed),
 #             mean_effective_speed = mean(effective_speed),
 #             mean_release_extension = mean(release_extension))
 # 
 # kmeans_dat2016_2018_10 <- dat_kmeans2016_2018 %>% 
 #   pluck("clusters", 6) %>% unnest() %>% select(cluster) %>% 
 #   bind_cols(model_dat2016_2018)
 # 
 # 
 # kmeans_dat2016_2018_10 %>% 
 #   group_by(cluster) %>% 
 #   summarise(mean_spin = mean(release_spin_rate),
 #             mean_release_speed = mean(release_speed),
 #             mean_effective_speed = mean(effective_speed),
 #             mean_release_extension = mean(release_extension))
 # 
 # 
 # dat_kmeans2016_2018 %>% select(nclust, within_ss) %>% 
 #   kable(digits = 3)
 # 
 # wss2016_2018 <- dat_kmeans2016_2018 %>%
 #   select(nclust, within_ss) %>%
 #   unnest(within_ss)
 # 
 # 
 # ggplot(wss2016_2018) + 
 #   geom_line(aes(nclust, within_ss))
 # 
 # grid.arrange(grobs = dat_kmeans2016_2018$plots)
 # 




# Hclust ------------------------------------------------------------------

# dat_hclust_scaled_full <- tibble(xmat = list(model_scaled_full %>% select(-description))) %>% 
#   crossing(method = c("complete"))
# 
# dat_hclust_scaled_full <- dat_hclust_scaled_full %>%
#   mutate(hcl = map2(xmat, method, run_hclust), # get dendos
#          dendo = map(hcl, ggdendrogram)) # plot dendos
# 
# dat_hclust_scaled_full <- dat_hclust_scaled_full %>%
#   crossing(ncuts = c(5:6)) %>%
#   mutate(clusters = map2(hcl, ncuts, cut_hclust), # Cut dendo
#          clust_dat = map2(xmat, clusters, get_cluster), # Get cluster info
#          plots = map(clust_dat, plot_cluster), # Plot clusters
#          titles = map2(method, ncuts, paste), # Get cluster labels
#          plots_labeled = map2(plots, titles, label_plot)) %>% # Label cluster plots
#   arrange(ncuts)
# 
# 
# hclust_full5 <- dat_hclust_scaled_full %>% 
#   pluck("clusters", 1) %>% as_tibble() %>% rename(cluster = value) %>% 
#   bind_cols(model_dat)
# 
# hclust_full6 <- dat_hclust_scaled_full %>% 
#   pluck("clusters", 2) %>% as_tibble() %>% rename(cluster = value) %>% 
#   bind_cols(model_dat)
# 
# 
# 
# dat_hclust_scaled_full %>%
#   filter(ncuts %in% c(5:6)) %>%
#   pluck("plots_labeled") %>%
#   grid.arrange(grobs = ., ncol = 1)


