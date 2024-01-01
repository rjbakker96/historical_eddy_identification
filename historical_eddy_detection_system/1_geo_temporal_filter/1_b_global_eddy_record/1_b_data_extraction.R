# global eddy tracks data extraction and formatting  

# author: Roderick Bakker

# date: 2023-09-12

# version: 1.0

# load libraries ----------------------------------------------------------

library(data.table)
library(ncdf4)
library(tidyverse)
library(lubridate)


# import data -------------------------------------------------------------

# import aviso/mercator historical eddy data from ncdf4 files for cyclonic and anticyclonic eddies

eddy_filenames <- list.files( # set pathname for global record of cyclonic eddies
  path = "1_geo_temporal_filter/1_b_global_eddy_record/input",
  full.names = TRUE
)

data_eddy <- map( # open nc files in R
  .x = eddy_filenames,
  .f = ~ nc_open(filename = .x)
) %>% 
  set_names(nm = c("anticyclonic", "cyclonic")) # set names of list elements


# set variables -----------------------------------------------------------

# set names of relevant parameters to be extracted from the dataset

# possible eddy parameters
eddy_variables_options <- names(data_eddy[[1]]$var)

# parameter description and units as in AVISO product handbook META3.2 DT (issue: 1.0, publication date: 2022-02-15)
eddy_variables_select <- c( 
  "latitude", 
  "longitude", 
  "time",
  "observation_number", 
  "track", 
  "amplitude", 
  "speed_average",
  "effective_radius"
)

# set latitude and longitude values for broad scale filter

latitude_filter <- c( # set values for latitude filter, 31.7 +- 4°
  27.7, 35.7
)

longitude_filter <- c( # set values for longitude filter, (-64.2 + 360) +-10°
  285.8, 305.8
)


# extract data ------------------------------------------------------------

anticyclonic <- map( # extract variables from netcdf file based on variable names
  .x = eddy_variables_select,
  .f = ~ ncvar_get(nc = data_eddy$anticyclonic, varid = .x)
) %>% 
  set_names(nm = eddy_variables_select) %>% # set names for list items
  as_tibble() %>% # transform list items to tibble
  filter( # filter on preset long & lat values
    between(latitude,
            latitude_filter[1], latitude_filter[2]) &
      between(longitude,
              longitude_filter[1], longitude_filter[2])
  ) %>% # reformat time column
  mutate(type = "anticyclonic",
         time = as_date(time, origin = "1950-01-01")) %>% 
  rename(date = time)


cyclonic <- map( # extract variables from netcdf file based on variable names
  .x = eddy_variables_select,
  .f = ~ ncvar_get(nc = data_eddy$cyclonic, varid = .x)
) %>% 
  set_names(nm = eddy_variables_select) %>% # set names for list items
  as_tibble() %>% # transform list items to tibble
  filter( # filter on preset long & lat values
    between(latitude,
            latitude_filter[1], latitude_filter[2]) &
    between(longitude,
            longitude_filter[1], longitude_filter[2])
  ) %>% # reformat time column
  mutate(type = "cyclonic",
         time = as_date(time, origin = "1950-01-01")) %>% 
  rename(date = time)

# turn on gc() function below to free working memory if needed
gc() # free working memory after taxing operations


# extract eddy origins ----------------------------------------------------

# anticyclonic eddies
eddy_track_anti <- anticyclonic$track %>% 
  unique() # extract unique eddy tracks ids for anticyclonic eddies

orig_anti <- map( # extract variables from nc file
  .x = c("latitude", "longitude", "observation_number", "track"),
  .f = ~ ncvar_get(nc = data_eddy$anticyclonic, varid = .x)
) %>% # set names for list items
  set_names(nm = c("latitude", "longitude", "observation_number", "track")) %>% 
  as_tibble() %>% # transform list items to tibble
  filter(observation_number == 0 & # filter for initial observation (origin)
           track %in% eddy_track_anti) %>% 
  rename( # rename to indicate coordinates are from initial observation (origin)
    lat_origin = latitude,
    long_origin = longitude
  ) %>% 
  select(!observation_number)

# cyclonic eddies
eddy_track_cyc <- cyclonic$track %>% 
  unique() # extract unique eddy tracks ids for cyclonic eddies

orig_cyc <- map( # extract variables from nc file
  .x = c("latitude", "longitude", "observation_number", "track"),
  .f = ~ ncvar_get(nc = data_eddy$cyclonic, varid = .x)
) %>% # set names for list items
  set_names(nm = c("latitude", "longitude", "observation_number", "track")) %>% 
  as_tibble() %>% # transform list items to tibble
  filter(observation_number == 0 & # filter for initial observation (origin)
           track %in% eddy_track_cyc) %>% 
  rename( # rename to indicate coordinates are from initial observation (origin)
    lat_origin = latitude,
    long_origin = longitude
  ) %>% 
  select(!observation_number)


# turn on gc() function below to free working memory if needed
gc() # free working memory after taxing operations

anticyclonic <- anticyclonic %>% 
  left_join(orig_anti, # join anticyclonic origins to eddy tracks
            by = "track")

cyclonic <- cyclonic %>% 
  left_join(orig_cyc, # join cyclonic origins to eddy tracks
            by = "track")


# join datasets -----------------------------------------------------------

eddy_full <- full_join(
  anticyclonic,
  cyclonic
)


# reformat for plotting and export ----------------------------------------

eddy_full <- eddy_full %>% 
  separate(col = date, # separate date column into year, month and day for downstream grouping
           into = c("year", "month", "day"), remove = FALSE) %>% 
  mutate(across(.cols = c(year, month, day),
                .fns = as.numeric)) %>% 
  mutate(effective_radius = effective_radius / 1000 / 111.1) %>% # transform radius in m to degrees (°)
  mutate(across(.cols = c(longitude, long_origin), # transform 360 east to 180 east-west
                .fns = ~ case_when(.x > 180
                                   ~ .x - 360,
                                   TRUE ~ .x))) %>% 
  select(type, date, year, month, day, observation_number, track, # select desired variables, also allows for reorganization of data frame
         latitude, longitude, amplitude, speed_average, effective_radius,
         lat_origin, long_origin)


# summarize to individual eddies ------------------------------------------

eddy_sums <- eddy_full %>% # summarize eddies per month
  group_by(type, track, year, month) %>% 
  summarise(across(.cols = everything(),
                   .fns = mean)) %>% 
  ungroup() %>% 
  select(type, date, year, month, day, observation_number, track, # select desired variables, also allows for reorganization of data frame
         latitude, longitude, amplitude, speed_average, effective_radius,
         lat_origin, long_origin)


# qc plots ----------------------------------------------------------------

# plot unique eddies per year
p_1_yearly <- eddy_sums %>% 
  select(type, track, year) %>% 
  unique() %>% 
  ggplot(aes(x = year))+
  geom_hline(yintercept = 0)+
  geom_bar(aes(fill = type),
           position = "dodge", alpha = 0.7)+
  labs(title = "1b eddy counts, yearly")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_1_yearly)

# plot eddy activity per month
p_2_monthly <- eddy_sums %>% 
  select(type, month) %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = month))+
  geom_hline(yintercept = 0)+
  geom_bar(aes(fill = type),
           position = "dodge", alpha = 0.8)+
  labs(title = "1b eddy counts, monthly")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_2_monthly)

# plot eddy origins per month
p_3_origins <- eddy_sums %>% 
  select(type, track, date, lat_origin, long_origin) %>% 
  unique() %>% 
  group_by(type, track, lat_origin, long_origin) %>% 
  summarise(month = min(date)) %>% # summarise to first month of occurrence according to date column.
  mutate(month = month(month)) %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = long_origin, y = lat_origin, 
             colour = type, shape = type))+
  geom_point(alpha = 0.7)+
  geom_point(x = -64.2, y = 31.7, # turn on for fixed point reference
             shape = 13, size = 4, colour = "hotpink")+
  geom_rect(aes(
    xmax = longitude_filter[1] - 360, xmin = longitude_filter[2] - 360,
    # xmax = longitude_filter[1], xmin = longitude_filter[2], # turn on for positive longitudes
    ymin = latitude_filter[1], ymax = latitude_filter[2]),
            fill = NA, colour = "hotpink")+
  facet_wrap(facets = ~ month,
             nrow = 3)+
  labs(title = "1b eddy origins")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13))

print(p_3_origins)


# export data files -------------------------------------------------------

# uncomment the lines below to export data and plots

# export files

# fwrite(x = eddy_full,
#        file = "./1_geo_temporal_filter/1_b_global_eddy_record/output/1_b_eddy_tracks_full.csv",
#        dateTimeAs = "write.csv")
# 
# fwrite(x = eddy_sums,
#        file = "./1_geo_temporal_filter/1_b_global_eddy_record/output/1_b_eddy_tracks_sums.csv",
#        dateTimeAs = "write.csv")


# export qc plots ---------------------------------------------------------

# 
# # uncomment lines below to export qc plots
# 
# # make list of qc plots
# 
# plot_list <- mget(ls(pattern = "^p_.*."))
# 
# # set file paths for qc plots
# 
# plot_list_names <- paste("./1_geo_temporal_filter/1_b_global_eddy_record/qc_plots/",
#                          names(plot_list),
#                          ".png", sep = "")
# 
# # export using ggsave and parallel mapping
# 
# map2(.x = plot_list, .y = plot_list_names,
#      .f = ~ ggsave(plot = .x, filename = .y,
#                    width = 8.6, height = 6.7, dpi = 120))


