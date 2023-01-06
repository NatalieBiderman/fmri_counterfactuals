create_csv_from_js_data <- function(individual_data_folder, preprocessed_data_folder, file_name, columns, main_trial, save_raw_data){
  
  # If packages are not installed, install. Then, load libraries. 
  list_of_packages <- c("dplyr") 
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(list_of_packages, require, character.only = TRUE)
  
  # Load data and concatenate into a single data frame
  #exp_data <- read.csv(sprintf("%s/%s",individual_data_folder,data_files[1]), stringsAsFactors = FALSE)
  # for (i in 2:length(data_files)){
  #   curr_data <- read.csv(sprintf("%s/%s",individual_data_folder,data_files[i]), stringsAsFactors = FALSE)
  #   if (!setequal(colnames(curr_data), colnames(exp_data))){
  #     print(paste0("error detected for",data_files[i]))
  #     curr_data[setdiff(names(exp_data), names(curr_data))] <- NA
  #   }
  #   exp_data <- rbind(exp_data, curr_data)
  # }
  data_files <- list.files(individual_data_folder,pattern="*.csv")
  # exp_data <- do.call(rbind, lapply(data_files,function(x) read.csv(sprintf("%s/%s",individual_data_folder,x), stringsAsFactors = FALSE)))
  exp_data <- c()
  for (i in 1:length(data_files)){
    curr_data <- read.csv(sprintf("%s%s",individual_data_folder,data_files[i]), stringsAsFactors = FALSE)
    exp_data <- rbind(exp_data, curr_data)
  }
  
  # turn rt columns to number
  exp_data$rt <- as.numeric(exp_data$rt)/1000
  exp_data$rt_pairs <- as.numeric(exp_data$rt_pairs)/1000
  exp_data$rt_object <- as.numeric(exp_data$rt_object)/1000
  exp_data$see_reward_rt <- as.numeric(exp_data$see_reward_rt)/1000
  exp_data$register_reward_rt <- as.numeric(exp_data$register_reward_rt)/1000
  exp_data$outcome_eval_rt <- as.numeric(exp_data$outcome_eval_rt)/1000
  exp_data$outcome_eval_confidence_rt <- as.numeric(exp_data$outcome_eval_confidence_rt)/1000
  
  # Save raw data 
  if (save_raw_data==1){
    write.csv(exp_data, file = sprintf("%s/Raw_data/raw_data.csv",preprocessed_data_folder))
  }
  
  # Choose specific columns 
  df <- exp_data[exp_data$ttype == main_trial,columns]
  
  # save
  #write.csv(df, file = sprintf("%s/df_%s",preprocessed_data_folder, file_name))
  
  return(df)
}