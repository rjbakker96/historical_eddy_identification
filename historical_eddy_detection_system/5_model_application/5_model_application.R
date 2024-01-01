# pattern recognition application

# Author: Roderick Bakker 

# Date: 2023-09-12

# Version: 1.0

# libraries ---------------------------------------------------------------

library(data.table)
library(caret)
library(randomForest)
library(MLmetrics)
library(tidyverse)


# import data -------------------------------------------------------------

eddies <- read_csv(
  file = "5_model_application/input/4_data_eddies_final.csv"
)

anomalies <- read_csv(
  file = "5_model_application/input/2_data_bootstrap_outcomes_summary.csv"
)


# format data -------------------------------------------------------------

eddies <- eddies %>% # select relevant columns
  select(year, month, type) 

anomalies <- anomalies %>% # filter for pattern recognition model input
  select(year, month, depth, func, density, salinity, temperature) %>%  # select for desired columns
  filter(func %in% c("min", "max", "median") & # filter for function outcomes
           depth %in% c(400, 500, 600, 700, 800, 900) # filter for depths
  ) %>% 
  filter(year <= 2020) # omit irrelevant data

# make sure the amount of observations doesn't change after joining operations
anomalies_eddies <- anomalies %>% # join eddy consensus to anomalies
  left_join(eddies,
            by = c("year", "month")) %>% 
  mutate(type = case_when(is.na(type) # contingency to catch any observations missed in previous scripts
                          ~ "non_eddy",
                          TRUE ~ type)) %>% 
  relocate(type, .before = depth)

anomalies_eddies_model <- anomalies_eddies %>% # reformat for pattern recognition model
  pivot_wider(names_from = c(depth, func), # pivot on available depths and function outcomes 
              values_from = c(density, salinity, temperature)) %>% 
  mutate(type = as_factor(type)) # set eddy types as factor for pattern recognition

anomalies_eddies_model_training <- anomalies_eddies_model %>% # select training data
  filter(year >= 1993 & year <= 2020)

anomalies_eddies_model_prediction <- anomalies_eddies_model %>% # select prediction data
  filter(year < 1993) %>% 
  arrange(month) %>% # arrange data to make sure it'll match up with prediction outcomes
  arrange(year)


# run model predictions ---------------------------------------------------

# set predictors
predictors <- anomalies_eddies_model %>% # extract names of predictors
  select(4:length(anomalies_eddies_model)) %>% 
  names() %>% 
  as_vector()

training_formula <- as.formula( # define formula for model training
  paste("type ~", paste(predictors, collapse = "+")))

control_formula <- trainControl( # define formula for model controls
  method = "repeatedcv", number = 10, repeats = 10,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  index = createResample(anomalies_eddies_model_training$type, times = 10))

mod <- train( # train the model
  training_formula , 
  data = anomalies_eddies_model_training[3:length(anomalies_eddies_model_training)], 
  method = "rf", trControl = control_formula,
  na.action = na.roughfix)

predictions <- predict( # apply the model to prediction data
  mod, 
  newdata = anomalies_eddies_model_prediction[4:length(anomalies_eddies_model_prediction)], 
  type = "prob",
  na.action = na.roughfix) %>% 
  as_tibble()

# extract highest likelyhood prediction
predictions_max <- tibble(predictions, # combine model outcomes with year and month columns
                      anomalies_eddies_model_prediction[1:2]) %>% 
  pivot_longer(cols = c(non_eddy, cyclonic, anticyclonic),
               names_to = "prediction", values_to = "prob") %>% 
  group_by(year, month) %>% 
  summarise(prob = max(prob)) %>% # extract max probability outcomes
  ungroup() %>% 
  mutate(prediction = colnames(predictions)[max.col(predictions)]) # extract column name with maximum probability

# join max likelihood prediction to model outcomes
data_eddies_predicted <- tibble(predictions_max, predictions)

# join anomaly data to predictions
data_eddies_predicted_anomalies <- data_eddies_predicted %>% 
  left_join(anomalies,
            by = c("year", "month"))


# qc_plots ----------------------------------------------------------------

# plot anomalies of predicted eddies over time
p_1_anomalies <- data_eddies_predicted_anomalies %>% 
  mutate(depth = as_factor(depth)) %>% 
  mutate(date = ymd(paste(year, month, 15, sep = "-"))) %>% # set date as mean of the month for plotting
  ggplot(aes(x = date, y = temperature))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = prediction))+
  facet_grid(rows = vars(depth), scales = "free")

print(p_1_anomalies)

# plot counts of months with eddy activity
p_2_counts <- data_eddies_predicted %>% # join predictions and final consensus for plotting
  select(year, month, prediction) %>% 
  rename(type = prediction) %>% 
  full_join(eddies) %>% 
  filter(type == "anticyclonic" | type == "cyclonic") %>% # filter for mesoscale eddy periods 
  mutate(type = factor(type, levels = c("anticyclonic", # set eddy types as factors for plotting
                                        "cyclonic"))) %>% 
  unique() %>% # contingency to drop duplicate months if present
  ggplot(aes(x = year, fill = type))+
  geom_vline(xintercept = (max(data_eddies_predicted$year) - 0.5), # extract max year for dividing line
             linetype = 2)+
  geom_bar(colour = "black")+
  geom_text(aes(label = after_stat(count)), stat = "count", 
            position = "stack", vjust = 2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(1988, 2020, 4),
                     minor_breaks = seq(1988, 2020, 1))+
  scale_fill_manual(values = c("#F8766D", "#619CFF"))+
  ylab(Eddy ~ Count)+
  theme(axis.title.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        panel.border = element_rect(fill = NA, 
                                    colour = "coral4", linewidth = 1))

print(p_2_counts)



# export data -------------------------------------------------------------

# # uncomment lines below to export eddy predictions
# 
# fwrite(x = data_eddies_predicted,
#        file = "5_model_application/output/5_eddies_predicted.csv")
# 
# fwrite(x = data_eddies_predicted_anomalies,
#        file = "5_model_application/output/5_eddies_predicted_anomalies.csv")

# export qc plots ---------------------------------------------------------

# # uncomment lines below to export qc plots
# 
# # make list of qc plots
# 
# plot_list <- mget(ls(pattern = "^p_.*."))
# 
# # set file paths for qc plots
# 
# plot_list_names <- paste("./5_model_application/qc_plots/",
#                          names(plot_list),
#                          ".png", sep = "")
# 
# # export using ggsave and parallel mapping
# 
# map2(.x = plot_list, .y = plot_list_names,
#      .f = ~ ggsave(plot = .x, filename = .y,
#                    width = 8.6, height = 6.7, dpi = 120))


