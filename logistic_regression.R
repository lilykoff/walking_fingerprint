
library(tidyverse)
training_data <- new_training_data3
get_important_grids <- function(training_data, threshold){
  imp_grids <- list()
  for(i in 1:32){
    imp_names <- training_data %>% filter(subject_id==i) %>% pivot_longer(cols = -subject_id) %>% 
      left_join(., names_key, by=c("name"="gc_name" ))%>% group_by(name, u) %>% 
      summarize(value = sum(value)) %>% group_by(u) %>% mutate(
          group_sum = sum(value)
        ) %>% ungroup() %>% mutate(
          pct = value/group_sum
        ) %>% filter(pct >= threshold ) %>% dplyr::select(name)  %>% unlist()
    imp_grids[[i]] <- unname(imp_names)
  }
  return(imp_grids)
}

fit_models <- function(imp_grids, training_data, testing_data){
  output_preds <- list()
  output_predgrids <- list()
  for(i in 1:32){
    
    grids <- imp_grids[[i]]
    training_tmp <- training_data %>% dplyr::select(c(subject_id, all_of(grids)))
    testing_tmp <- testing_data %>% dplyr::select(c(subject_id, all_of(grids)))
    training_tmp <- training_tmp %>% mutate(
      class = ifelse(subject_id == i, 1, 0)
    )
    testing_tmp <- testing_tmp %>% mutate(
      class = ifelse(subject_id == i, 1, 0)
    )
    model <- glm(class ~., data = training_tmp[,-1], family = binomial(link ="logit"))
    # get most important grids 
    pvals <- summary(model)$coefficients[,"Pr(>|z|)"][-1]
    top3 <- names_1pct[c(which(pvals==sort(pvals)[1]), which(pvals==sort(pvals)[2]), which(pvals==sort(pvals)[3]))][1:3]
    names(top3) <- c("1", "2", "3")
    output_predgrids[[i]] <- top3
    
    # predict probability that each row belongs to subject i 
    pred <- predict(model, training_tmp[,-1], type = "response")
   
    # store predictions in another matrix where column i is the predicted probs for subject i
    output_preds[[i]] <- data.frame(cbind(preds = pred, subj = testing_tmp$subject_id))
    output_predgrids[[i]] <- top3
  }
  return(list(predictions = output_preds, most_imp_girds = output_predgrids))
}

# get subjects predicted identities 
get_predicted_identity <- function(output_preds){
  meanpreds <- matrix(NA, nrow = 32, ncol = 32)
  for(i in 1:32){
    tmp <- output_preds[[i]]
    tmp <-  tmp %>% group_by(subj) %>% summarize(
      meanpred = mean(preds)
    )
    meanpreds[i, ] <- tmp$meanpred
  }
  meanpreds <- data.frame(meanpreds)
  # i, jth entry of meanpreds is avg. probability that row a row of data from subject i is 
  # identified as subject j 
  meanpreds$subject_id <- seq(1, 32,1)
  meanpreds_long <- meanpreds %>% pivot_longer(cols = 1:32)
  colnames(meanpreds_long) <- c("true_subj", "test_subject", "prob")
  regexp <- "[[:digit:]]+"
  meanpreds_long <- meanpreds_long %>% mutate(
    test_subject_num = as.numeric(str_extract(test_subject, regexp)),
    match = ifelse(true_subj == test_subject_num, 1, 0),
    match2 = ifelse(match==1, "Correct Subject", "Not Correct Subject")
  )
  results_all <- meanpreds_long %>% group_by(true_subj) %>% summarize(
    maxprob = max(prob),
    prediction = test_subject_num[prob==maxprob],
    probsubj = prob[true_subj==test_subject_num]
  ) %>% mutate(
    correct = ifelse(as.numeric(prediction)==true_subj, 1, 0)
  )
  return(list(mean_predictions = meanpreds_long, results = results_all))
}



