# geo-temporal filter using water profiles and eddy tracks, output for shiny app

# author: Roderick Bakker

# date: 2023-09-01

# version: 1.0

# libraries ---------------------------------------------------------------


library(data.table)
library(tidyverse)


# import data -------------------------------------------------------------

profiles <- read_csv(
  file = "1_geo_temporal_filter/input/1_a_data_profiles_filt_select.csv"
)

data_eddy <- read_csv(
  file = "1_geo_temporal_filter/input/1_b_eddy_tracks_full.csv"
)



# calculate eddy lifespans ------------------------------------------------

# calculate lifespans from eddy tracks 1_a output (needed for downstream filter)
eddy_lifespan <- data_eddy %>% 
  select(type, track, observation_number) %>% 
  group_by(type, track) %>% 
  summarise(lifespan = max(observation_number, na.rm = TRUE)) %>% 
  filter(lifespan < 5) # filter for eddies with a lifespan of less than 5 days



# apply coordinates filter for data reduction -----------------------------


profiles_coordinates <- profiles %>% # extract coordinates per individual cast
  select(cruise, cast, year, month, day, longitude, latitude) %>% 
  unique() %>% 
  rename(long_profile = longitude, lat_profile = latitude) 

mean_coordinates <- c( # calculate mean coordinates of all casts
  mean(profiles_coordinates$lat_profile, na.rm = TRUE),
  mean(profiles_coordinates$long_profile, na.rm = TRUE)
)

data_eddy <- data_eddy %>% 
  filter( # filter eddy tracks in 2*2 box around mean coordinates
    between(latitude,
            mean_coordinates[1] - 1, mean_coordinates[1] + 1) &
    between(longitude,
            mean_coordinates[2] - 1, mean_coordinates[2] + 1)
    )

# summarize parameters and counts per eddy type
data_eddy_parameters <- data_eddy %>% 
  group_by(type, track) %>% # split into two summary functions to count individual eddies
  summarise(across(.cols = c(longitude, latitude, # initial summary on individual eddies
                             amplitude, effective_radius, speed_average),
                   .fns = mean, na.rm = TRUE)) %>% 
  summarise(across(.cols = c(longitude, latitude, # second summary per eddy type
                             amplitude, effective_radius, speed_average),
                   .fns = mean, na.rm = TRUE),
            n = n()) 
  
# summarize to individual eddies per month for qc plots
eddy_plots_raw <- data_eddy %>% 
  group_by(type, track, year, month) %>% 
  summarise(across(.cols = everything(),
                   .fns = mean)) %>% 
  ungroup() %>% 
  select(type, date, year, month, day, observation_number, track, # select desired variables, also allows for reorganization of data frame
         latitude, longitude, amplitude, speed_average, effective_radius,
         lat_origin, long_origin)


# qc plots 1 --------------------------------------------------------------

# plot eddy tracks
p_1_a_eddy_tracks <- data_eddy %>% 
  arrange(date) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot(aes(x = longitude, y = latitude, colour = type))+
  geom_point(aes(x = mean_coordinates[2], y = mean_coordinates[1]),
             shape = 13, size = 4, colour = "hotpink")+
  geom_path(aes(group = track))+
  facet_wrap(~ year)+
  labs(title = "1 all eddy tracks")

print(p_1_a_eddy_tracks)

# plot eddy origins per month
p_2_a_eddy_origins <- data_eddy %>% 
  select(type, track, date, lat_origin, long_origin) %>% 
  unique() %>% 
  group_by(type, track, lat_origin, long_origin) %>% 
  summarise(month = min(date)) %>% # summarise to first month of occurance according to date column.
  mutate(month = month(month)) %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = long_origin, y = lat_origin, 
             colour = type, shape = type))+
  geom_point(alpha = 0.7)+
  geom_point(x = mean_coordinates[2], y = mean_coordinates[1], # turn on for fixed point reference
             shape = 13, size = 4, colour = "hotpink")+
  geom_rect(aes(
    xmin =  mean_coordinates[2] + 1, xmax =  mean_coordinates[2] - 1,
    ymin =  mean_coordinates[1] - 1, ymax =  mean_coordinates[1] + 1),
    fill = NA, colour = "hotpink")+
  facet_wrap(facets = ~ month,
             nrow = 3)+
  labs(title = "1 all eddy origins")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_2_a_eddy_origins)

# plot unique eddies per year
p_3_a_yearly <- eddy_plots_raw %>% 
  select(type, track, year) %>% 
  unique() %>% 
  ggplot(aes(x = year))+
  geom_hline(yintercept = 0)+
  geom_bar(aes(fill = type),
           position = "dodge", alpha = 0.7)+
  labs(title = "1 all eddy counts, yearly")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_3_a_yearly)

# plot eddy activity per month
p_4_a_monthly <- eddy_plots_raw %>% 
  select(type, month) %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = month))+
  geom_hline(yintercept = 0)+
  geom_bar(aes(fill = type),
           position = "dodge", alpha = 0.8)+
  labs(title = "1 all eddy counts, monthly")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_4_a_monthly)


# initial filter ----------------------------------------------------------

# calculate days spent in 2*2 area for each eddy
eddy_days_spent <- data_eddy %>% 
  select(type, track) %>%
  group_by(type, track) %>% 
  summarise(days_spent = n()) %>% 
  filter(days_spent < 5) # filter for eddies that spent less than 5 days around BATS


# drop eddies based on initial filters
data_eddy_filt <- data_eddy %>% 
  anti_join(eddy_days_spent, # filter out eddies that spent less than 5 days in 2*2 area
            by = c("type", "track")) %>%
  anti_join(eddy_lifespan, # filter out eddies with lifespan less than 5 days
            by = c("type", "track")) %>% 
  ungroup() %>% 
  as_tibble()


# apply geo-temporal filter -----------------------------------------------

data_eddy_filt <- data_eddy_filt %>% 
  left_join(profiles_coordinates, # join cast coordinates to eddy tracks data
            by = c("year", "month", "day"))

data_eddy_filt <- data_eddy_filt %>% 
  mutate(distance = sqrt( # calculate distance between eddy center and casts
    (lat_profile - latitude)^2 + (long_profile - longitude)^2
  ))

# summarize parameters per eddy type
data_eddy_parameters_filt <- data_eddy_filt %>% 
  group_by(type, track) %>% 
  summarise(across(.cols = c(longitude, latitude, 
                             amplitude, effective_radius, speed_average,
                             distance),
                   .fns = mean, na.rm = TRUE)) %>% 
  summarise(across(.cols = c(longitude, latitude, 
                             amplitude, effective_radius, speed_average,
                             distance),
                   .fns = mean, na.rm = TRUE),
            n = n()) 

eddy_interactions <- data_eddy_filt %>% 
  filter(effective_radius > distance) %>% # filter for station occupation during eddy passing
  select(type, track) %>% 
  unique()

data_eddy_initial_concensus <- data_eddy_filt %>% 
  semi_join(eddy_interactions, # filter for eddies which passed when station was occupied
            by = c("type", "track")) 


# summarize parameters per eddy type
data_eddy_parameters_initial_consensus <- data_eddy_initial_concensus %>% 
  group_by(type, track) %>% 
  summarise(across(.cols = c(longitude, latitude, 
                             amplitude, effective_radius, speed_average,
                             distance),
                   .fns = mean, na.rm = TRUE)) %>% 
  summarise(across(.cols = c(longitude, latitude, 
                             amplitude, effective_radius, speed_average,
                             distance),
                   .fns = mean, na.rm = TRUE),
            n = n()) 

# summarize to individual eddies per month for qc plots
eddy_plots_initial_concensus <- data_eddy_initial_concensus %>% 
  group_by(type, track, year, month) %>% 
  summarise(across(.cols = everything(),
                   .fns = mean)) %>% 
  ungroup() %>% 
  select(type, date, year, month, day, observation_number, track, # select desired variables, also allows for reorganization of data frame
         latitude, longitude, amplitude, speed_average, effective_radius,
         lat_origin, long_origin)


# qc plots 2 --------------------------------------------------------------

# plot eddy tracks
p_1_b_eddy_tracks <- data_eddy_initial_concensus %>% 
  arrange(date) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot(aes(x = longitude, y = latitude, colour = type))+
  geom_point(aes(x = mean_coordinates[2], y = mean_coordinates[1]),
             shape = 13, size = 4, colour = "hotpink")+
  geom_path(aes(group = track))+
  facet_wrap(~ year)+
  labs(title = "1 initial concensus eddy tracks")

print(p_1_b_eddy_tracks)

# plot eddy origins per month
p_2_b_eddy_origins <- data_eddy_initial_concensus %>% 
  select(type, track, date, lat_origin, long_origin) %>% 
  unique() %>% 
  group_by(type, track, lat_origin, long_origin) %>% 
  summarise(month = min(date)) %>% # summarise to first month of occurance according to date column.
  mutate(month = month(month)) %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = long_origin, y = lat_origin, 
             colour = type, shape = type))+
  geom_point(alpha = 0.7)+
  geom_point(x = mean_coordinates[2], y = mean_coordinates[1], # turn on for fixed point reference
             shape = 13, size = 4, colour = "hotpink")+
  geom_rect(aes(
    xmin =  mean_coordinates[2] + 1, xmax =  mean_coordinates[2] - 1,
    ymin =  mean_coordinates[1] - 1, ymax =  mean_coordinates[1] + 1),
    fill = NA, colour = "hotpink")+
  facet_wrap(facets = ~ month,
             nrow = 3)+
  labs(title = "1 initial concensus eddy origins")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_2_b_eddy_origins)

# plot unique eddies per year
p_3_b_yearly <- eddy_plots_initial_concensus %>%
  select(type, track, year) %>% 
  unique() %>% 
  ggplot(aes(x = year))+
  geom_hline(yintercept = 0)+
  geom_bar(aes(fill = type),
           position = "dodge", alpha = 0.7)+
  labs(title = "1 initial concensus eddy counts, yearly")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_3_b_yearly)

# plot eddy activity per month
p_4_b_monthly <- eddy_plots_initial_concensus %>% 
  select(type, month) %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = month))+
  geom_hline(yintercept = 0)+
  geom_bar(aes(fill = type),
           position = "dodge", alpha = 0.8)+
  labs(title = "1 initial concensus eddy counts, monthly")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_4_b_monthly)


# export data files -------------------------------------------------------

# # make list of data files to export
# 
# file_list_data_files <- mget(x = ls(pattern = "^data_.*."))
# 
# # make list of data filepaths where to export to
# 
# path_list_data_files <- paste("./1_geo_temporal_filter/output/",
#                               "1_",
#                               names(file_list_data_files), ".csv",
#                               sep = "")
# 
# 
# # export using fwrite and parallel mapping
# 
# map2(.x = file_list_data_files, .y = path_list_data_files,
#      .f = ~ fwrite(x = .x, file = .y, dateTimeAs = "write.csv"))

# export qc plots ---------------------------------------------------------


# # uncomment lines below to export qc plots
# 
# # make list of qc plots
# 
# plot_list <- mget(ls(pattern = "^p_.*."))
# 
# # set file paths for qc plots
# 
# plot_list_names <- paste("./1_geo_temporal_filter/qc_plots/",
#                          names(plot_list),
#                          ".png", sep = "")
# 
# # export using ggsave and parallel mapping
# 
# map2(.x = plot_list, .y = plot_list_names,
#      .f = ~ ggsave(plot = .x, filename = .y,
#                    width = 8.6, height = 6.7, dpi = 120))
