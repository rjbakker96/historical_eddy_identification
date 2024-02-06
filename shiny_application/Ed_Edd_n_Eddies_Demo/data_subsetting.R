# title: data subsetting for demo application
#
# date: 2024-01-14
#
# author: Roderick Bakker
#

# 0) notes ----------------------------------------------------------------

#
#
#


# 1) libraries ------------------------------------------------------------

library(data.table)
library(tidyverse)



# 2: import data -------------------------------------------------------------

data_names <- list.files( # import file paths
  path = "./GitHub/shiny_application/Eddy_Analysis/data/",
  pattern = ".*.csv$",
  full.names = TRUE, 
  recursive = TRUE
)

# 3: format data -------------------------------------------------------------

data_files <- data_names %>% 
  map(.f = read_csv, show_col_types = FALSE) %>% # import all csv files
  set_names(nm = c("profiles", "profiles_mean", "eddy_tracks", "anomalies", "map_coords")) %>% # set names on each file 
  map_if( # if date column is present, format date column as dates
    .p = ~ "date" %in% names(.x),
    .f = ~ mutate(.x, date = as_date(date))
  ) %>% 
  map_if( # if month column is present, format month as abbreviations and set as factors
    .p = ~ "month" %in% names(.x),
    .f = ~ mutate(.x, month = factor(month.abb[month], levels = month.abb))
  ) %>% 
  map_if( # if eddy tracks column is present, format eddy tracks as factors
    .p = ~ "track" %in% names(.x),
    .f = ~ mutate(.x, track = as_factor(track))
  )

# filter files with date column for a single year of data
data_files_filt <- data_files[1:4] %>% 
  map_if(
    .p = ~ "date" %in% names(.x),
    .f = ~ filter(.x, date >= ymd("2008-01-01") & date <= ymd("2008-12-31"))
  )

# secondary data filter and selection

# select relevant data for profiles
data_files_filt[[1]] <- data_files_filt[[1]] %>% 
  select(cruise, cast, date, month, day, 
         longitude, latitude, depth, 
         temperature, salinity, density, oxygen)

# select relevant data for mean references
data_files_filt[[2]] <- data_files_filt[[2]] %>% 
  select(month, depth, temperature, salinity, density, oxygen)

# select relevant data for anomalies
## anomalies doesn't filter on dates, need a separate setup for this
## check if this has fixed itself in the hydrostation version
data_files_filt[[4]] <- data_files_filt[[4]] %>% 
  filter(year == 2008 & func %in% c("min", "max", "median"))


# 4 export data -----------------------------------------------------------

# map2(
#   .x = data_files_filt,
#   .y = data_names[1:4],
#   .f = ~ fwrite(x = .x,
#                 file = paste("./GitHub/shiny_application/Ed_Edd_n_Eddies_Demo/data_subset",
#                              str_extract(.y, pattern = "[:digit:].*.csv"),
#                              sep = "/"),
#                 dateTimeAs = "write.csv")
# )

