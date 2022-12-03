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