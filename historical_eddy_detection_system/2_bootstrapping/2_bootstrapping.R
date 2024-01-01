# bootstrapping for eddy detection system

# author: Roderick Bakker

# date: 2023-09-01

# version: 1.0

# load libraries ----------------------------------------------------------

library(data.table)
library(tidyverse)
library(slider)
library(tictoc)

# import data -------------------------------------------------------------

profiles <- read_csv(
  "2_bootstrapping/input/1_a_data_profiles_filt_select.csv"
)


# format data -------------------------------------------------------------

profiles <- profiles %>% # select desired variables
  select(year, month, depth, 
         temperature, salinity, density, oxygen)
  
depths_filter <- c( # set desired depths for bootstrapping
  #seq(0, 140, 10),
  #seq(150, 400, 50),
  seq(100, 1000, 100)
)

profiles <- profiles %>% # filter for desired depths
  filter(depth %in% depths_filter)

profiles <- profiles %>% # calculate monthly means of all casts per year and depth
  group_by(month, year, depth) %>% 
  summarise(across(.cols = everything(),
                   .fns = mean, na.rm = TRUE)) %>% 
  ungroup()

profiles <- profiles %>% # pivot on desired variables
  pivot_longer(cols = c(
    temperature, salinity, density, oxygen
  ), names_to = "variables", values_to = "values")

profiles_nested <- profiles %>% # nest data per month, variables and depth
  mutate(year_index = ymd(paste(year, "01-01", sep = "-"))) %>% # set year as a date object for slider indexing
  arrange(year) %>% # arrange by years
  group_by(month, variables, depth) %>% 
  nest() %>% # nest to lists
  ungroup()


# bootstrapping -----------------------------------------------------------


anomaly_calculation <- function(.data){ # define function for climate anomaly calculations
  
  n <- nrow(.data) # count amount of observations
  
  # extract random sample as index and use to extract values from data
  sample_index <- sample(1:n, 1)
 
  # set amount of random samples to select for monthly mean calculation
  if(n > 7) { # if more than 7 years are available
    n_samples <- 7 # set random amount of samples to 7
  } else { # if 7 or less are available 
    n_samples <- n - 1 # take the amount of samples minus 1
  }
  
  # select random samples from data, excluding the initial sample
  random_index <- sample(setdiff(1:n, sample_index), n_samples)
  
  # calculate delta value for randomly selected year
  delta <- .data$values[sample_index] - mean(.data$values[random_index], na.rm = TRUE)
  
  # return data frame with delta value and the corresponding year
  data.frame(delta = delta, year = .data$year[sample_index])
  
}


n <- 150 # set amount of replicates for each window

tic() # monitor duration of bootstrapping procedure

data_bootstrap <- profiles_nested %>% # apply bootstrapping procedure
  mutate(
    booties = map(.x = data,
                  .f = ~ slide_period_dfr(.x = .x, # apply function in sliding window to each list
                                          .i = .x$year_index,
                                          .period = "year",
                                          .step = 5,
                                          .f = ~
                                            replicate(n, # repeat function application n times
                                                      expr = anomaly_calculation(.data = .x), 
                                                      simplify = FALSE) %>% 
                                            reduce(.f = full_join, by = c("delta", "year")), # join outcomes
                                          .before = 7, .after = 8,
                                          .complete = FALSE))
  ) %>% 
  ungroup() %>% 
  select(!data) %>% 
  unnest(booties) # unnest data to return outcomes as a dataframe

toc() -> time_passed # return duration of bootstrapping procedure

# turn on gc() command to free working memory if needed
gc() # free working memory


# extract and format summary values ---------------------------------------

data_bootstrap_sums <- data_bootstrap %>% # summarize bootstrap values
  group_by(year, month, variables, depth) %>% 
  summarise(min = min(delta, na.rm = TRUE),
            max = max(delta, na.rm = TRUE),
            median = median(delta, na.rm = TRUE),
            mean = mean(delta, na.rm = TRUE)) %>% 
  drop_na()

data_bootstrap_sums <- data_bootstrap_sums %>% # format bootstrap values
  pivot_longer(cols = c(min, max, median, mean), # pivot on type of function applied
               names_to = "func", values_to = "anomalies") %>% 
  pivot_wider(names_from = variables, values_from = anomalies) # pivot on variables

# qc plots ----------------------------------------------------------------

# plot bootstrap summary values along depths
p_1_deltas <- data_bootstrap_sums %>% 
  pivot_longer(cols = c(density, salinity, temperature, oxygen), # pivot on functions
               names_to = "variables", values_to = "anomalies") %>% 
  mutate(month = month.abb[month], # format months as abbreviations and set as factor
         month = factor(month, levels = month.abb)) %>% 
  ggplot(aes(x = depth, y = anomalies, colour = month, linetype = func))+
  geom_hline(yintercept = 0)+
  geom_smooth(se = FALSE)+
  facet_grid(cols = vars(variables),
             scales= "free")+
  coord_flip()+
  scale_x_continuous(breaks = depths_filter, # use depth filter setting as plot breaks
                     trans = "reverse")

print(p_1_deltas)

# calculate bootstrap distributions of anomalies for one month of several years along depth
booty_counts <- data_bootstrap %>% 
  group_by(year, month, variables, depth) %>% 
  filter(month == 6 & year %in% c(2007, 2008, 2009)) %>% # filter for month and years
  summarise(count = hist(delta, breaks = 40, plot = FALSE)$counts, # generate distribution counts using histogram plot outcomes
            delta = hist(delta, breaks = 40, plot = FALSE)$mids) %>%  # generate distribution values
  ungroup() %>% 
  mutate(month = month.abb[month],
         month = factor(month, levels = month.abb)) 

# plot bootstrap distributions
p_2_distributions <- booty_counts %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot(aes(x = delta, y = count, colour = year, fill = year))+
  geom_vline(xintercept = 0, linetype = 2, colour = "black")+
  geom_col()+
  geom_hline(yintercept = 0)+
  geom_smooth(se = FALSE, method = "gam")+
  facet_grid(rows = vars(depth),
             cols = vars(variables),
             scales = "free")

print(p_2_distributions)



# export data files -------------------------------------------------------

# # uncomment lines below to export qc plots
# 
# fwrite(data_bootstrap,
#        file = "./2_bootstrapping/output/2_data_bootstrap_outcomes.csv",
#        dateTimeAs = "write.csv")
# 
# fwrite(data_bootstrap_sums,
#        file = "./2_bootstrapping/output/2_data_bootstrap_outcomes_summary.csv",
#        dateTimeAs = "write.csv")


# export qc plots ---------------------------------------------------------

# # uncomment lines below to export qc plots
# 
# # make list of qc plots
# 
# plot_list <- mget(ls(pattern = "^p_.*."))
# 
# # set file paths for qc plots
# 
# plot_list_names <- paste("./2_bootstrapping/qc_plots/10-1500/",
#                          names(plot_list),
#                          ".png", sep = "")
# 
# # export using ggsave and parallel mapping
# 
# map2(.x = plot_list, .y = plot_list_names,
#      .f = ~ ggsave(plot = .x, filename = .y,
#                    width = 8.6, height = 6.7, dpi = 120))
