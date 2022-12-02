library(tidyverse)

df_fit <- `raw_accel_data_deocs copy`
time_lags <- seq(15, 90, 15)
train_split <- 200
test_split <- 180
gcell_size <- 0.25

create_train_test(df_fit = df_fit, time_lags = time_lags, gcell_size = gcell_size,
                  train_split  =  200, test_split = test_split)
