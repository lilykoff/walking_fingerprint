library(tidyverse)

# set parameters for processing the data
# data can be downlaoded from https://physionet.org/content/accelerometry-walk-climb-drive/1.0.0/
data_path <- "./data"


df_fit <- readRDS(file.path(data_path,"raw_accel_data_deocs.rds"))

# here we use six time lags, cell grid size of 0.25x0.25 m^2/s^4
# and 200:180 split for the training and testing
time_lags <- seq(15, 90, 15)
train_split <- 200
test_split <- 180
gcell_size <- 0.25

data <- create_train_test(df_fit = df_fit, time_lags = time_lags, gcell_size = gcell_size,
                  train_split  =  train_split, test_split = test_split)
# extract data 
train <- data.frame(data$train)
test <- data.frame(data$test)
names_key <- data.frame(data$key)
# save training and testing data 
write.csv(train, "train.csv")
write.csv(test, "test.csv")
write.csv(names_key, "names_key.csv")

# set threshold for grid importance, i.e. which grids to keep 
threshold <- 0.001 
# get list of important grids that will be used in each subject's logistic regression 
imp_grids <- get_important_grids(training_data = train, threshold = threshold, key = names_key)

# fit 32 one vs. rest logistic regression models 
predicted_output <- fit_models(imp_grids = imp_grids, training_data = train, testing_data = test) 

# get predicted identities from results 

model_results <- get_predicted_identity(predicted_output)

# visualization 
plot_predicted_probs(model_results$mean_predictions, save = T)
model_results$results


