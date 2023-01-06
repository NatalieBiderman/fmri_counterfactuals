create_interactive_csv_from_js_data <- function(int_data_folder, exp_data_folder, ttype_name){
  
  # If packages are not installed, install. Then, load libraries. 
  list_of_packages <- c("dplyr") 
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(list_of_packages, require, character.only = TRUE)
  
  # Load data and concatenate into a single data frame
  int_data_files <- list.files(sprintf("%s/",int_data_folder),pattern="*.csv")
  #int_data <- do.call(rbind, lapply(int_data_files,function(x) read.csv(sprintf("%s/%s",int_data_folder,x), stringsAsFactors = FALSE)))
  int_data <- do.call(rbind, lapply(int_data_files,function(x) tryCatch(read.csv(sprintf("%s/%s",int_data_folder,x), stringsAsFactors = FALSE),error=function(e) NULL)))
  
  # add task type 
  int_data[ttype_name] <- NaN 
  
  # use raw data to find the ttype during events 
  exp_data_files <- list.files(sprintf("%s/",exp_data_folder),pattern="*.csv")
  exp_data <- do.call(rbind, lapply(exp_data_files,function(x) read.csv(sprintf("%s/%s",exp_data_folder,x), stringsAsFactors = FALSE)))
  
  for (i in 1:nrow(int_data)){
    # find index 
    logical_vector = exp_data$PID == int_data$PID[i] & exp_data$trial_index == int_data$trial[i];
    if (any(logical_vector)){
      int_data[i,ttype_name] = exp_data[exp_data$PID == int_data$PID[i] & exp_data$trial_index == int_data$trial[i],ttype_name]
    }
  }
  
  return(int_data)
}