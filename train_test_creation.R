# df_fit <- readRDS("raw_accel_data.rds")
library(tidyverse)


# function takes as input full data, vector of time lags, grid cell size, num of seconds for training, number of seconds for testing
# vector of time lags: each entry must be between 0 and 100 
# gcell size must be between 0 and 3 and 3 must be divisible by the number 
# train_split  + test_split must equal 380
# output: training data, testing data, and grid cell number key 


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

time_lags <- c(15, 30)
train_split <- 200
test_split <- 180
# create_train_test(df_fit=df_fit, time_lags=time_lags, train_split=200, test_split = 180, gcell_size=1)
