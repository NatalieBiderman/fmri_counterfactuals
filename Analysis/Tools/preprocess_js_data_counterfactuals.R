
preprocess_js_data_counterfactuals <- function(raw_data_path,preprocessed_data_folder){
  
  # If packages are not installed, install. Then, load libraries. 
  list_of_packages <- c("dplyr","tibble") 
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(list_of_packages, require, character.only = TRUE)
  
  # ----------------------------------------------------------------------------
  # Create csvs for the different tasks 
  # ----------------------------------------------------------------------------
  
  ratings_columns = c("PID", "time_elapsed","index", "stimulus_id", "painting", "response","rt");
  deliberation_columns = c("PID","time_elapsed","index","block","reward_type","stimulus_left","stimulus_right","rating_left","rating_right","painting_left","painting_right","left_chosen","chosen_obj","unchosen_obj","explain_trial");
  reward_learning_columns = c("PID","time_elapsed","index","block","stimulus_id","reward_type","reward_amount","see_reward_rt");
  final_decisions_columns = c("PID","time_elapsed","index","block","rt","chosen_trial","gain_left","left_chosen","higher_outcome_chosen","chosen_obj","unchosen_obj","stimulus_left","stimulus_right","rating_left","rating_right","painting_left","painting_right","stimulus_left_deliberation_ind","stimulus_right_deliberation_ind");
  memory_columns = c("PID","time_elapsed","index","stimulus_left","stimulus_right","old_pair","old_response","rt_pairs","chosen_object");
  outcome_eval_columns = c("PID","time_elapsed","index","stimulus_id","reward_type","chosen_obj","initial_rating","outcome_eval_response","outcome_eval_rt","outcome_eval_gain","outcome_eval_acc");
  final_ratings_columns = c("PID", "time_elapsed","index", "stimulus_id", "painting","deliberated_stim", "reward_type","chosen_obj","initial_rating","new_rating","rt");
  
  df_ratings <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "ratings.csv",
    columns = ratings_columns,
    main_trial = "rating",
    save_raw_data = 1)

  df_deliberation <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "deliberation.csv",
    columns = deliberation_columns,
    main_trial = "deliberation",
    save_raw_data = 0)
  
  df_reward_learning <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "reward_learning.csv",
    columns = reward_learning_columns,
    main_trial = "see_reward",
    save_raw_data = 0)
  
  df_final_decisions <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "final_decisions.csv",
    columns = final_decisions_columns,
    main_trial = "final_decisions",
    save_raw_data = 0)
  
  df_memory <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "memory.csv",
    columns = memory_columns,
    main_trial = "memory_pairs",
    save_raw_data = 0)
  
  df_outcome_evaluation <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "outcome_evaluation.csv",
    columns = outcome_eval_columns,
    main_trial = "outcome_evaluation",
    save_raw_data = 0)
  
  df_final_ratings <- create_csv_from_js_data(
    individual_data_folder,
    preprocessed_data_folder,
    file_name = "final_ratings.csv",
    columns = final_ratings_columns,
    main_trial = "final_ratings",
    save_raw_data = 0)
  
  # ----------------------------------------------------------------------------
  # Preprocess data files
  # ----------------------------------------------------------------------------
  
  all_data <- read.csv(sprintf("%s/Raw_data/raw_data.csv",preprocessed_data_folder))
  
  # ------------------------------- Deliberation -------------------------------
  
  # add explain responses
  df_deliberation_explain <- all_data %>% 
    subset(ttype == "explain_trial") %>%
    dplyr::select("PID", "explain_response", "rt", "index") %>%
    rename(explain_rt = rt) %>%
    mutate(explain_response = str_remove(gsub("[[:punct:]]", "", explain_response),"Q0"))
  
  df_deliberation <- df_deliberation %>%
    merge(df_deliberation_explain, by=c("PID","index"))
  
  
  # ----------------------------- Reward learning ------------------------------
  
  # add reward registration
  
  df_reward_registration <- all_data %>%
    subset(ttype == "reward_outcome") %>%
    dplyr::select("PID", "index","register_reward_response", "register_reward_rt") 
  
  df_reward_learning <- df_reward_learning %>%
    merge(df_reward_registration, by=c("PID","index")) %>%
    mutate(register_reward_acc = ifelse(register_reward_response == reward_type, 1, 0))
  
  # ---------------------------------- Memory ----------------------------------
  
  # add object memory test 
  
  df_object_memory <- all_data %>%
    subset(ttype == "memory_chosen_object") %>%
    dplyr::select("PID", "index","left_object_chosen", "rt_object") 
  
  df_memory <- df_memory %>%
    merge(df_object_memory, by=c("PID","index")) %>%
    mutate(object_choice_response = ifelse(left_object_chosen == 1, stimulus_left, stimulus_right),
           object_choice_acc = ifelse(old_pair == 0, NaN, ifelse(object_choice_response == chosen_object, 1, 0))) %>%
    dplyr::select(-left_object_chosen)
  
  # ---------------------------- Outcome evaluation ----------------------------
  
  # add confidence trials 
  
  df_confidence <- all_data %>%
    subset(ttype == "outcome_evaluation_confidence") %>%
    dplyr::select("PID", "index", "outcome_eval_confidence",	"outcome_eval_confidence_rt") 
  
  df_outcome_evaluation <- df_outcome_evaluation %>%
    merge(df_confidence, by=c("PID","index"))
  
  # ----------------------------- Demographic info -----------------------------
  
  # get debreif questions 
  raw_debreif <- all_data %>%
    subset(grepl("debreif", ttype)) %>% 
    dplyr::select(c("PID", "ttype", "responses"))
  
  debreif <- raw_debreif %>%
    mutate(responses = gsub("[[:space:]]", " ",str_remove(gsub("[[:punct:]]", "", responses),"Q0"))) %>%
    mutate(new_ttype = str_remove(as.character(ttype),"debreif_")) %>%
    dplyr::select(-ttype) %>%
    spread(new_ttype, responses) %>%
    dplyr::select(-c("end", "intro"))
  
  # -------------------------- Interactive data frame --------------------------

  int_data <- create_interactive_csv_from_js_data(
    int_data_folder = sprintf("%s/Interactive_data/",raw_data_path), 
    exp_data_folder = sprintf("%s/Individual_data/",raw_data_path),
    ttype_name = "ttype")
  
  # ----------------------------------------------------------------------------
  # Save all data files
  # ----------------------------------------------------------------------------
  
  write.csv(df_deliberation, file = sprintf("%s/df_deliberation.csv",preprocessed_data_folder))
  write.csv(df_reward_learning, file = sprintf("%s/df_reward_learning.csv",preprocessed_data_folder))
  write.csv(df_ratings, file = sprintf("%s/df_ratings.csv",preprocessed_data_folder))
  write.csv(df_final_decisions, file = sprintf("%s/df_final_decisions.csv",preprocessed_data_folder))
  write.csv(df_memory, file = sprintf("%s/df_memory.csv",preprocessed_data_folder))
  write.csv(df_outcome_evaluation, file = sprintf("%s/df_outcome_evaluation.csv",preprocessed_data_folder))
  write.csv(df_final_ratings, file = sprintf("%s/df_final_ratings.csv",preprocessed_data_folder))
  write.csv(debreif, file = sprintf("%s/df_debreif.csv",preprocessed_data_folder))
  write.csv(int_data, file = sprintf("%s/interaction.csv",preprocessed_data_folder))
  
}