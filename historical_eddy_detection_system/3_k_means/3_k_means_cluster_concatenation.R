# K-means cluster outcome data concatenation

# Author: Roderick Bakker

# date: 2023-09-05


# libraries ---------------------------------------------------------------

library(data.table)
library(tidyverse)


# import data -------------------------------------------------------------

# list file names of cluster outcomes
filenames <- list.files(path = "./3_k_means/output/monthly/",
                        full.names = TRUE)

# concatenate files
data_clusters <- filenames %>% 
  map(.f = read_csv) %>% 
  reduce(.f = full_join)


# export data -------------------------------------------------------------

# # export concatenated cluster outcomes
# fwrite(data_clusters, 
#        file = "./3_k_means/output/3_data_eddies_clusters_initial.csv",
#        dateTimeAs = "write.csv")
