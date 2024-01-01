# profiles data preparation for downstream analysis and shiny app

# author: Roderick Bakker

# date: 2023-09-12

# version: 1.0


# libraries ---------------------------------------------------------------

library(data.table)
library(oce)
library(tidyverse)
library(lubridate)


# import data -------------------------------------------------------------

# import profiles ctd data
profiles_headers <- c( # set header names for columns
  "index_1", "index_2", "cruise", "date", "latitude", "longitude",
  "pressure", "depth", "temperature", "salinity",
  "oxygen", "BAC", "fluorescence", "PAR"
)

data_profiles <- read_csv( # import profiles data
  file = "1_geo_temporal_filter/1_a_water_profiles/input/data_concat.csv",
  col_names = profiles_headers, col_types = c("nnnnnnnnnnnnnn"), 
  na = c("-999", "-999.0", "-999.00", "-999.000"),
  skip = 1
)


# calculate densities -----------------------------------------------------

data_profiles <- data_profiles %>% # calculate densities
  mutate(longitude = longitude * -1) %>% # set longitude as negative value (West)
  mutate(
    density = swSigma0( # calculate potential density
      salinity = salinity, temperature = temperature, pressure = 0,
      longitude = longitude, latitude = latitude,
      eos = "gsw")
  )


# format data -------------------------------------------------------------

data_profiles <- data_profiles %>% # format columns
  mutate(date = date_decimal(date)) %>% # set decimal year as datetime object
  separate(col = cruise, # separate into cruise and cast column
           into = c("cruise", "cast"),
           sep = 5, convert = TRUE) %>% 
  separate(col = date,
           into = c("date", "time"),
           sep = " ", convert = TRUE) %>% 
  separate(col = date, # separate date column into year, month and day
           into = c("year", "month", "day"),
           sep = "-", convert = TRUE, 
           remove = FALSE) %>% 
  select(cruise, cast, date, time, year, month, day, # select columns
         longitude, latitude, depth, pressure,
         temperature, salinity, density,
         oxygen, fluorescence, BAC, PAR)

# filter data -------------------------------------------------------------

# set latitude and longitude filters

latitude_filter <- c( # set values for latitude filter
  31.55, 31.85
)

longitude_filter <- c( # set values for longitude filter
  -64.3, -64.05
)

depth_filter <- c( # set values for depth filter
  0, 1500
)


p_1_coords <- data_profiles %>% # use coordinates plot to adjust lat & long filters
  select(latitude, longitude, year) %>% 
  unique() %>% # extract only unique coordinates to reduce amount of data to be plotted
  ggplot(aes(x = longitude, y = latitude, colour = year))+
  geom_rect(
    xmax = longitude_filter[1], xmin = longitude_filter[2], # turn on for positive longitudes
    ymin = latitude_filter[1], ymax = latitude_filter[2],
    fill = NA, colour = "hotpink", linewidth = 3)+
  geom_point(alpha = 0.1)+
  scale_colour_gradientn(colours = rainbow(7))+
  labs(title = "1a cast coordinates and filter")

print(p_1_coords)

# apply filters to formatted data

data_profiles_filt <- data_profiles %>% 
  filter( # filter on preset long, lat & depth values
      between(latitude, # latitude filter
              latitude_filter[1], latitude_filter[2]) &
      between(longitude, # longitude filter
              longitude_filter[1], longitude_filter[2]) &
      between(depth, # depth filter
              depth_filter[1], depth_filter[2])
  )


# bin data ----------------------------------------------------------------

# activate lines in this chapter if data averaging is desired. 
data_profiles_filt <- data_profiles_filt %>% # bin every 10m 
  mutate(depth = round(depth, digits = -1)) %>% # round depth to nearest 10m
  group_by(cruise, cast, date, time, year, month, day, longitude, latitude, depth) %>% 
  summarise(across(.cols = everything(), # bin across all variables
                   .fns = mean, na.rm = TRUE)) %>% 
  ungroup() # removing grouping 

# turn on gc() function below to free working memory if needed
gc() # free working memory after taxing operations


# filter for deep casts ---------------------------------------------------

profiles_filt <- data_profiles_filt %>% # extract deep casts (>=500m)
  select(cruise, cast, depth) %>% 
  group_by(cruise, cast) %>% 
  summarise(depth = max(depth, na.rm = TRUE)) %>% # calculate max depths for each cast
  filter(depth >= 500) %>% # filter for deep casts (>= 500m)
  select(cruise, cast) # extract cruise and cast numbers of deep casts 

data_profiles_filt_select <- data_profiles_filt %>% # filter for deep casts (>=500m) in original data
  semi_join(profiles_filt)

# calculate means ---------------------------------------------------------

data_profiles_means <- data_profiles_filt_select %>% # calculate monthly means
  select(month, depth, pressure, # select variables to be averaged
         temperature, salinity, density, 
         oxygen, fluorescence, BAC, PAR) %>% 
  group_by(month, depth) %>% 
  summarise(across(.cols = everything(), # calculate monthly averages
                   .fns = mean, na.rm = TRUE)) %>% 
  ungroup() # remove grouping


# qc plots ----------------------------------------------------------------

p_2_means <- data_profiles_means %>% # plot mean values for all variables
  pivot_longer(cols = c(temperature, salinity, pressure, density, 
                        oxygen, fluorescence, BAC, PAR),
               names_to = "vars", values_to = "values") %>% 
  mutate(month = as_factor(month)) %>% # set months as factor for colour coding
  ggplot(aes(x = depth, y = values, colour = month))+
  geom_line()+
  facet_wrap(~ vars, # facet on variables (vars)
             scales = "free")+
  coord_flip()+ # flip x and y axis
  scale_x_continuous(trans = "reverse")+ # reverse depth on y-axis (remember x and y are flipped)
  labs(title = "1a monthly means")

print(p_2_means)

p_3_casts <- data_profiles_filt_select %>% # plot mean values against casts
  mutate(year = as_factor(year)) %>% 
  ggplot(aes(x = depth, y = temperature, colour = year))+
  geom_smooth(se = FALSE)+ # add smoothed mean lines through all casts per year
  geom_line(data = data_profiles_means,
            colour = "black", linetype = 2)+
  facet_wrap(~ month, # facet per month
             scales = "free")+
  coord_flip()+ # flip x and y axis
  scale_x_continuous(trans = "reverse")+ # reverse depth on y-axis (remember x and y are flipped)
  labs(title = "1a monthly means vs casts per year")


print(p_3_casts)
  


# export data files -------------------------------------------------------

# # uncomment lines below to export data files
# 
# # make list of data files to export
# 
# file_list_data_files <- mget(x = ls(pattern = "^data_.*."))
# 
# # make list of data filepaths where to export to
# 
# path_list_data_files <- paste("./1_geo_temporal_filter/1_a_water_profiles/output/",
#                               "1_a_",
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
# plot_list_names <- paste("./1_geo_temporal_filter/1_a_water_profiles/qc_plots/",
#                          names(plot_list),
#                          ".png", sep = "")
# 
# # export using ggsave and parallel mapping
# 
# map2(.x = plot_list, .y = plot_list_names,
#      .f = ~ ggsave(plot = .x, filename = .y,
#                    width = 8.6, height = 6.7, dpi = 120))
