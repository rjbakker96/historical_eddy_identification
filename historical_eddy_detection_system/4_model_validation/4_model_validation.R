# pattern recognition model validation

# Author: Roderick Bakker

# Date: 2023-09-06

# Version: 1.0

# libraries ---------------------------------------------------------------

library(data.table)
library(caret)
library(randomForest)
library(MLmetrics)
library(tidyverse)


# import data -------------------------------------------------------------

eddies <- read_csv(
  file = "4_model_validation/input/3_data_eddies_clusters_intermediary.csv"
)

# # uncomment to run validation with final consensus
# eddies <- read_csv(
#   file = "4_model_validation/output/4_data_eddies_final.csv"
# )

anomalies <- read_csv(
  file = "4_model_validation/input/2_data_bootstrap_outcomes_summary.csv"
)


# format data -------------------------------------------------------------

eddies <- eddies %>% # select relevant columns
  select(year, month, type)

anomalies <- anomalies %>% # filter for pattern recognition model input
  select(year, month, depth, func, density, salinity, temperature) %>%  # select for desired columns
  filter(year >= 1993 & year <= 2020 &  # filter for years with satellite altimetry data
           func %in% c("min", "max", "median") & # filter for function outcomes
           depth %in% c(400, 500, 600, 700, 800, 900) # filter for depths
         ) %>% 
  drop_na()

# should have same amount of observations as original anomalies data set
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
  mutate(type = as_factor(type)) %>%  # set eddy types as factor for pattern recognition
  arrange(month) %>%
  arrange(year)


# model predictions -------------------------------------------------------

# define pattern recognition function
eddy_identifier_training <- function(.data, .predictors){
  
  data_train <- .data %>% # set training data
    slice_sample(prop = 0.9)
  
  data_predict <- .data %>% # set prediction data 
    anti_join(data_train) %>% # select all data which isn't in the training data
    arrange(month) %>% # arrange data to make sure it'll match up after predictions
    arrange(year)
  
  
  training_formula <- as.formula( # define formula for model training
    paste("type ~", paste(.predictors, collapse = "+")))
  
  control_formula <- trainControl( # define formula for model controls
    method = "repeatedcv", number = 10, repeats = 10,
    classProbs = TRUE,
    summaryFunction = multiClassSummary,
    index = createResample(data_train$type, times = 10))
  
  mod <- train( # train the model
    training_formula , data = data_train[3:length(data_train)], 
    method = "rf", trControl = control_formula,
    na.action = na.roughfix)
  
  predict <- predict( # apply the model to new data
    mod, newdata = data_predict[4:length(data_predict)], # should eddy types be included here? don't think so!!
    type = "prob",
    na.action = na.roughfix) %>% 
    as_tibble()
  
  predict <- tibble(predict, data_predict[1:3]) %>% 
    pivot_longer(cols = c(non_eddy, cyclonic, anticyclonic),
                 names_to = "prediction", values_to = "prob") %>% 
    group_by(year, month, type) %>% 
    summarise(prob = max(prob)) %>% # extract max probability outcomes
    ungroup() %>% 
    mutate(prediction = colnames(predict)[max.col(predict)]) # add month used for filtering to predictions
  
  predict_stats <- predict %>% 
    filter(year >= 1993)
  
  # calculate quality metrics
  accuracy <- (predict_stats$type == predict_stats$prediction) %>% 
    sum(na.rm = TRUE) / nrow(predict_stats) * 100 # accuracy of the predictions
  
  positive <- predict_stats %>% 
    filter(type == "non_eddy" & prediction != "non_eddy") %>% 
    nrow() / nrow(predict_stats) * 100 # amount of false positives as percentage
  
  
  negative <- predict_stats %>% 
    filter(type != "non_eddy" & prediction == "non_eddy") %>% 
    nrow() / nrow(predict_stats) * 100 # amount of false negatives as percentage
  
  probability <- round( 
    (mean(predict_stats$prob) * 100), # average probability score
    digits = 2)
  
  list(tibble(accuracy, positive, negative, probability),
       predict) # join all data as list for export
  
}

# set as save function as contingency 
eddy_identifier_training <- safely(eddy_identifier_training)


# run model ---------------------------------------------------------------

# set predictors
predictors <- anomalies_eddies_model %>% # extract names of predictors
  select(4:length(anomalies_eddies_model)) %>% 
  names() %>% 
  as_vector()

# loop model function with random training and prediction data
models <- replicate(n = 200, 
                        expr = eddy_identifier_training(
                          .data = anomalies_eddies_model,
                          .predictors = predictors
                        ), simplify = FALSE) %>% 
  map("result") %>% # only return results list
  compact() # drop result lists without data

# extract and join model statistics
data_model_stats <- models %>%
  map(~ .x[[1]]) %>%
  reduce(.f = full_join)

data_model_stats <- data_model_stats %>% # add iteration number
  mutate(n = rep(1:nrow(data_model_stats), each = 1)) 

# summarise model statistics
data_model_stats_sums <- data_model_stats %>% 
  select(accuracy, positive, negative, probability) %>% 
  pivot_longer(cols = c(accuracy, positive, negative, probability),
               names_to = "stat", values_to = "values") %>% 
  group_by(stat) %>% 
  summarise(mean = mean(values),
            sd = sd(values))

# extract and join model outcomes
model_predict <- models %>%
  map(~ .x[[2]]) %>%
  reduce(.f = full_join)
 
# extract false-negative errors
negatives <- model_predict %>% 
  filter(type != "non_eddy" & prediction == "non_eddy") %>% 
  mutate(error = "negative")

# extract false-positive errors
positives <- model_predict %>% 
  filter(type == "non_eddy" & prediction != "non_eddy") %>% 
  mutate(error = "positive")

# join errors
data_model_errors <- full_join(negatives,
                               positives)

# summarise errors
data_model_errors_sum <- data_model_errors %>% 
  group_by(year, month, error, prediction, type) %>% 
  summarise(n = n(), prob = mean(prob))



# export data -------------------------------------------------------------

# uncomment lines below to export data files

# make list of data files to export

file_list_data_files <- mget(x = ls(pattern = "^data_.*."))

# make list of data filepaths where to export to

path_list_data_files <- paste("./4_model_validation/output/stats_intermediary/",
                              "4_",
                              names(file_list_data_files),
                              "_intermediary",
                              ".csv",
                              sep = "")

# # export files using parallel mapping
# map2(.x = file_list_data_files, .y = path_list_data_files,
#      .f = ~ fwrite(x = .x, file = .y, dateTimeAs = "write.csv"))

