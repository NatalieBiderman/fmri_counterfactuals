create_event_log <- function(raw_data, events, task){
  
  # subset data
  event_df <- raw_data %>% 
    mutate(duration_time_elapsed = (time_elapsed - lag(time_elapsed, default = time_elapsed[1]))/1000) %>%
    subset(ttype %in% events) %>%
    dplyr::select(PID,index,block, ttype,rt,onset,duration,duration_time_elapsed) %>%
    dplyr::rename(event = "ttype") %>%
    mutate(onset_time_elapsed = NaN) %>%
    mutate(task = task) %>%
    subset(block != 0) # remove practice block
  
  # adapt event types
  event_df[event_df$event=="no_response" | grepl("warning", event_df$event), "event"] <- "no_response_prompt"
  event_df[grepl("choice",event_df$event) & !grepl("confirmation",event_df$event) & !grepl("warning",event_df$event) & is.na(event_df$rt), "event"] <- "no_response"
  event_df[grepl("confirmation",event_df$event), "event"] <- "confirmation"
  event_df[grepl("choice",event_df$event), "event"] <- "choice"
  
  # remove rt column
  event_df <- event_df %>%
    dplyr::select(-rt)
  
  # add onset based on time_elapsed
  event_df_full <- c()
  blocks <- unique(event_df$block)
  subs <- unique(event_df$PID)
  for (s in subs){
    for (b in blocks){
      event_df_block <- subset(event_df, block==b & PID == s) 
      event_df_block$onset_time_elapsed[1] = 0;
      event_df_block$onset_time_elapsed[2:nrow(event_df_block)] = cumsum(event_df_block$duration_time_elapsed[1:(nrow(event_df_block)-1)])
      event_df_full <- bind_rows(event_df_full, event_df_block)
    }
  }
  
  event_df_full <- event_df_full %>%
    dplyr::arrange(PID, task, block, index)
  
  return(event_df_full)
}

