library(tidyverse)

create_train_test <- function(df_fit, time_lags, gcell_size, train_split, test_split){
  if(!is.data.frame(df_fit)){
    return("Error: df_fit must be data frame")
  }
  if(!is.vector(time_lags)){
    return("Error: time lags must be vector")
  }
  if(!(all(time_lags > 0) & all(time_lags<100))){
    return("Error: time lags must be between0 and 100 not inclusive")
  }
  if(gcell_size <= 0 | gcell_size > 3){
    return("Error: grid cell size must be between 0 and 3")
  }
  if(3%%gcell_size != 0){
    return("Error: 3 must be divisible by grid cell size")
  }
  if(!is.numeric(train_split)|!is.numeric(test_split)|train_split < 0 | train_split>380| test_split < 0 | test_split>380 | train_split+test_split!=380){
    return("Error: train_split + test_split must be between 0 and 380 and both must sum to 180")
  }
  train <- subset(df_fit, J %in% 1:train_split)
  test <- subset(df_fit, J %in% (train_split+1):(train_split+test_split))
  
  interval <- train_split 
  
  df_1 <- list()
  df_2 <- list()
  num_cells <- (3/gcell_size)^2
  num_lags <- length(time_lags)
  
  for (j in 1:32) {
    df_1[[j]] <- data.frame(grid_id=seq(1:num_cells))
    df_1[[j]]$subject_id <- rep(j, num_cells)
    df_2[[j]] <- matrix(0, nrow=interval, ncol=(num_lags*num_cells)+1)
    for (t in 1:interval) {
      for (i in 1:length(time_lags)) {
        
        xdf1 <- xdf1t <- data.frame(time_group=seq(from=1, to=100, by=1), 
                                    signal=as.vector(t(subset(train, id == j)$smat_sub[t,])))
        xdf1t$time_group <- xdf1$time_group+time_lags[i]
        
        xdf2 <- inner_join(xdf1, xdf1t, by="time_group")
        names(xdf2) <- c("time_group", "s", "u")
        
        count <- xdf2 %>% mutate(cut_s = cut(s, breaks = seq(0, 3, by = gcell_size), include.lowest = T),
                                 cut_u = cut(u, breaks = seq(0, 3, by = gcell_size), include.lowest = T)) %>% 
          drop_na() %>%
          count(cut_s, cut_u, .drop=FALSE)
        
        df_1[[j]][,i+2] <- count$n
      } 
      df_2[[j]][t,] <- as.matrix(pivot_wider(df_1[[j]], id_cols = subject_id, names_from = grid_id, values_from = -c(grid_id, subject_id)),
                                 ncol=(num_lags*num_cells)+1)
    } 
    print(paste("Subject", j, "training completed"))
  }
  df_3 <- Reduce(rbind, df_2)
  u <- c()
  for(k in 1:num_lags){
    u <- append(u, c(rep(time_lags[k]/100, num_cells)))
  }
  names <- data.frame(v = rep("cg_", num_lags*num_cells), num  =seq(1,num_lags*num_cells, 1), s = rep(as.character(count$cut_s), num_lags), 
                      `s-u` = rep(as.character(count$cut_u), num_lags), u = u)
  names <- names %>% rowwise() %>% mutate(
    gc_name = paste(v, num, sep = "")
  )
  cnames <- names %>% dplyr::select(gc_name) %>% unlist()
  
  colnames(df_3) <- c("subject_id", cnames)
  
  names_key <- names
  training_data <- df_3
  
  interval <- test_split
  
  df_1 <- list()
  df_2 <- list()
  
  
  for (j in 1:32) {
    df_1[[j]] <- data.frame(grid_id=seq(1:num_cells))
    df_1[[j]]$subject_id <- rep(j, num_cells)
    df_2[[j]] <- matrix(0, nrow=interval, ncol=(num_lags*num_cells)+1)
    for (t in 1:interval) {
      for (i in 1:length(time_lags)) {
        
        xdf1 <- xdf1t <- data.frame(time_group=seq(from=1, to=100, by=1), 
                                    signal=as.vector(t(subset(test, id == j)$smat_sub[t,])))
        xdf1t$time_group <- xdf1$time_group+time_lags[i]
        
        xdf2 <- inner_join(xdf1, xdf1t, by="time_group")
        names(xdf2) <- c("time_group", "s", "u")
        
        count <- xdf2 %>% mutate(cut_s = cut(s, breaks = seq(0, 3, by = gcell_size), include.lowest = T),
                                 cut_u = cut(u, breaks = seq(0, 3, by = gcell_size), include.lowest = T)) %>% 
          drop_na() %>%
          count(cut_s, cut_u, .drop=FALSE)
        
        df_1[[j]][,i+2] <- count$n
      } 
      df_2[[j]][t,] <- as.matrix(pivot_wider(df_1[[j]], id_cols = subject_id, names_from = grid_id, values_from = -c(grid_id, subject_id)),
                                 ncol=(num_lags*num_cells)+1)
    } 
    print(paste("Subject", j, "testing completed"))
  }
  df_3 <- Reduce(rbind, df_2)
  colnames(df_3) <- c("subject_id", cnames)
  testing_data <- df_3
  return(list(train = training_data, test = testing_data, key = names_key))
}

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

