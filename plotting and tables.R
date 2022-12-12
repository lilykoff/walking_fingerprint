# plotting the predicted probabilities 
plot_predicted_probs(model_results$mean_predictions, save = F)
# looking at results 
model_results$results

# extracting some results to see which subjects are best predicted 
model_results$mean_predictions %>% group_by(test_subject_num) %>% summarize(
  highest_prob = max(prob),
  mean = mean(prob[prob!=highest_prob]),
  diff = highest_prob - mean
) %>% arrange(desc(diff)) %>% print(n=Inf)


# table 1 raw data 
data$train %>% as.data.frame() %>% 
  dplyr::select(subject_id, cg_1, cg_2, cg_210, cg_211, cg_864) %>% slice(1:5, 200, 201, 202, 6400) %>%
  mutate(subject_id = ifelse(subject_id == 1, 1, 0)) %>% 
  gt() %>% gtsave(., "sample_data.tex")

# figure 1
# change filepath here - can't upload raw data 
subject <- 21
filename <- real_id %>% filter(ID == subject) %>% dplyr::select(file) %>% unlist()
filepath <- "/Users/lilykoffman/Documents/Fingerprinting/wearables_special_topics_course/data/IU_walking_driving_climbing/raw_accelerometry_data/"
subject_data <- read.csv(paste(filepath, filename, sep = ""))
plot_rawdata(subject_data, 20)

# figure 2
raw_accel_data <- readRDS("~/Documents/raw_accel_data.rds")
df_fit <- raw_accel_data

plot_3D(subject, df_fit, time_lags=c(0.15, 0.3))

# counts 
plot_counts(subject, df_fit, time_lag=0.3)
  
