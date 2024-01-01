# K-means clustering for eddy detection system

# author: Roderick Bakker

# date: 2023-10-16

# version: 1.0

# libraries ---------------------------------------------------------------

library(data.table)
library(tidyverse)
library(ggfortify)
library(ggpubr)

# import data -------------------------------------------------------------

eddies <- read_csv( # import initial consensus from geo-temporal filter
  file = "./3_k_means/input/1_data_eddy_initial_concensus.csv"
)

anomalies <- read_csv( # import summarized bootstrap outcomes
  file = "./3_k_means/input/2_data_bootstrap_outcomes_summary.csv"
)


# format data -------------------------------------------------------------

eddies <- eddies %>% # select desired variables from initial consensus
  select(type, year, month, track) %>% 
  unique() # summarize to unique eddies per month

anomalies <- anomalies %>% # apply filters to bootstrap outcomes
  select(year, month, func, depth, density, salinity, temperature) %>% 
  filter(year >= 1993,
         func %in% c("median"),
         depth %in% c(500, 600, 700, 800)) 

data_clustering <- anomalies %>% # pivot for clustering
  pivot_wider(names_from = depth, values_from = c(density, salinity, temperature))

data_clustering <- data_clustering %>% # join eddy classifications to bootstrap outcomes
  left_join(eddies,
            by = c("year", "month"))

data_clustering <- data_clustering %>% # transform NA values to non_eddy
  mutate(type = case_when(is.na(type)
                          ~ "non_eddy",
                          TRUE ~ type))

data_clustering <- data_clustering %>% # rearrange columns
  relocate(type, .before = year) %>% 
  relocate(track, .before = year)


# apply K-means clustering ------------------------------------------------

# set month on which to apply the K-means clustering
month_filter <- 7

# extract data to be used for K-means clustering
k_input <- data_clustering %>% 
  filter(month == month_filter) %>% # filter on preset month
  arrange(year) %>% 
  select(6:length(data_clustering)) %>% # set start of selection to first data column
  drop_na()

# extract years on which clustering is to be applied
k_years <- data_clustering %>% 
  filter(month == month_filter) %>% # filter on preset month
  arrange(year) %>% 
  select(year, 6:length(data_clustering)) %>% # set start of selection to first data column
  drop_na() %>% 
  select(year)

# apply K-means clustering
k_clusters <- kmeans(
   x = k_input,
   centers = 7, iter.max = 20, nstart = 20
)

# extract clusters from K-means outcomes and join to original data
clusters <- k_years %>% 
  mutate(cluster = as_factor(k_clusters$cluster),
         month = month_filter) %>% # filter on preset month
  left_join(eddies, # join eddy types to cluster outcomes
            by = c("year", "month")) %>% 
  mutate(type = case_when(is.na(type) # set type as non_eddy if no data is present from the initial consensus 
                          ~ "non_eddy",
                          TRUE ~ type)) %>% 
  unique()

data_clusters <- clusters %>% 
  left_join(anomalies, # join bootstrapped anomaly values to cluster outcomes
            by = c("year", "month")) %>% 
  select(year, cluster, month, depth, func, type, track, 
         density, salinity, temperature) 


# qc plots ----------------------------------------------------------------

# set shapes for plotting eddy types
eddy_shapes <- c(2, 6, 8)
names(eddy_shapes) <- c("anticyclonic", "cyclonic", "non_eddy")

# set alpha values for plotting eddy types
eddy_alphas <- c(0.8, 0.8, 0.3)
names(eddy_alphas) <- c("anticyclonic", "cyclonic", "non_eddy")

# plot ts values along depth with cluster outcomes
p_1_ts_median <- data_clusters %>% 
  mutate(cluster = as_factor(cluster)) %>% # set clusters as factors
  ggplot(aes(x = salinity, y = temperature, colour = cluster, shape = type))+
  geom_hline(yintercept = 0, colour = "black")+
  geom_vline(xintercept = 0, colour = "black")+
  geom_point(alpha = 0.8)+
  facet_wrap(~ depth,
             nrow = 1)+
  scale_shape_manual(values = eddy_shapes)+ # adjust shapes to preset values
  scale_alpha_manual(values = eddy_alphas)+ # adjust alphas to preset values
  guides(colour = guide_legend(override.aes = list(alpha = 1), # adjust legend to display on a single row
                               nrow = 1))

print(p_1_ts_median)

# run PCA using same data as K-means clustering for plotting
PCA_k_means <- prcomp(
  k_input
  )
 
# plot PCA otucomes with K-means clusters
p_2_PCA <- autoplot(PCA_k_means,
                    data = clusters, 
                    colour = "cluster", shape = "type",
                    frame = TRUE, frame.type = "norm"
)+
  geom_text(aes(colour = cluster, label = clusters$cluster), # add text labels for clusters
            nudge_y = -0.05, alpha = 0.5)+
  geom_point(aes(colour = cluster, shape = type), # reformat data points
             size = 3, alpha = 0.5)+
  scale_shape_manual(values = eddy_shapes)+ # adjust shapes to preset values
  scale_alpha_manual(values = eddy_alphas)+ # adjust alphas to preset values
  guides(colour = guide_legend(override.aes = list(alpha = 1), # adjust legend to display on a single row
                               nrow = 1))

print(p_2_PCA)

# combine ts and PCA plots
p_3_ts_PCA <- ggarrange(
  p_1_ts_median,
  p_2_PCA,
  nrow = 2, heights = c(1, 1.6),
  common.legend = TRUE, legend = "bottom"
)

p_3_ts_PCA <- annotate_figure(p = p_3_ts_PCA, # add figure title indicating month
                              fig.lab = paste(month.abb[month_filter]),
                              fig.lab.pos = "top.left",
                              fig.lab.face = "bold",
                              fig.lab.size = 14)

print(p_3_ts_PCA)


# export data -------------------------------------------------------------

# # uncomment the lines below to export data
# 
# data_clusters_export <- clusters %>% # add column with abbreviated months for easier references
#   mutate(month_abb = month.abb[month]) %>% 
#   relocate(month_abb, .before = type)
# 
# fwrite(data_clusters_export, # export data by pasting file name with month number and abbreviation
#        file = paste("./3_k_means/output/monthly/",
#                     "data_clusters_",
#                     month_filter,
#                     "_",
#                     month.abb[month_filter],
#                     ".csv",
#                     sep = ""),
#        dateTimeAs = "write.csv")


# export qc plot ---------------------------------------------------------

# # uncomment the lines below to export plots
# 
# ggsave(plot = p_3_ts_PCA, # export ts-PCA plot by pasting file name with month number and abbrevitation
#        filename = paste("./3_k_means/qc_plots/monthly/",
#                         "p_ts_PCA_",
#                         month_filter,
#                         "_",
#                         month.abb[month_filter],
#                         ".png",
#                         sep = ""),
#        width = 9.5, height = 6.8, dpi = 300)


