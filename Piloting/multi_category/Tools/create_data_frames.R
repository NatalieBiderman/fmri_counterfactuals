# Code to create data frames for the different phases of the experiment 
# Written by Natalie Biderman, Feb 2, 2023

rm(list=ls(all=TRUE)) 

library(dplyr)
library(tidyr)
library(truncnorm)

# parameters 
n_scanned_designs = 1; # scanned versions will also have iti sampling in another code, in which we also generate four versions, so for now we only need one versions will all relevant columns and rows. 
n_not_scanned_designs = 4; 
categories = c("outdoor_scenes", "objects", "faces", "body_parts")
n_deliberation_pairs = 12;
n_reps_of_deliberation_pair_types = 2;
n_deliberation_blocks = 4;
n_deliberation_runs = 2;
mean_del_block1 = 2.6; sd_del_block1 = 0.9
mean_del_block2 = 1.7; sd_del_block2 = 0.6
mean_del_block3 = 1.2; sd_del_block3 = 0.5
mean_del_block4 = 1.2; sd_del_block4 = 0.5
max_scramble_time = 2; # max scramble prompt is 1 sec. together, scramble + prompt = 3
n_outcome_learning_rep = 6; # each item is repeated three times 
n_outcome_learning_runs = 2;
n_fd_blocks = 4;
n_fd_runs = 4;
n_memory_reps = 4;
n_memory_runs = 2;
n_outcome_estimation_reps = 4;
n_outcome_estimation_runs = 2;

# timing parameteres (note that these will be changed after iti sampling and randomization)
timing_ratings_iti = 500;
timing_deliberation_iti = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(4, 3, 2, 2, 1)) # taken from an exponential distribution, averaged to 1700 ms.
timing_outcome_learning_iti = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(4, 3, 2, 2, 1)) # taken from an exponential distribution, averaged to 1700 ms.
timing_outcome_learning_isi = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(4, 3, 2, 2, 1)) # taken from an exponential distribution, averaged to 1700 ms.
timing_final_decisions_iti = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(14, 10, 6, 4, 2))
timing_memory_iti = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(4, 3, 2, 2, 1))
timing_outcome_estimation_iti = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(4, 3, 2, 2, 1))

# load stims and use only selected categories
exp_stims <- read.csv("../Stimuli/Stim_csv/experimental_stimuli.csv")
exp_stims <- subset(exp_stims, category %in% categories) %>% dplyr::select(-X)
practice_stims <- read.csv("../Stimuli/Stim_csv/practice_stimuli.csv")

# ==============================================================================
# Localizer
# ==============================================================================

n_stims_per_block = 20;
stim_duration = 400;
iti_duration = 300;
timing_baseline_block = n_stims_per_block * (stim_duration+iti_duration) # 14 secs 
n_reps_per_category = 3;
total_exp_time = length(categories)*2*timing_baseline_block*n_reps_per_category/60000
n_stims_per_cat = n_stims_per_block*n_reps_per_category;
odd_ball_percent = 0.1 

loc_stims <- read.csv("../Stimuli/Stim_csv/localizer_stimuli.csv")
prac_loc_stims <- practice_stims %>% subset(phase == "Localizer")

# practice trials
loc_practice_trials <- data.frame(
  PID = NaN,
  practice = 1,
  trial = NaN,
  run = 0,
  task = NaN,
  stimulus_duration = stim_duration,
  iti = iti_duration,
  is_odd_ball = c(0,0,1,0,1,0),
  path = prac_loc_stims$path,
  response = NaN,
  rt = NaN
)

# we run four versions of this phase so we could randomize the order of trials and categories. 
for (i in 1:n_not_scanned_designs){
  
  # we shuffle order or blocks, but we want to make sure the same block doesn't repeat twice
  keep.looking = 1
  all_blocks = rep(categories, n_reps_per_category)
  while (keep.looking == 1){
    all_blocks = sample(all_blocks)
    # The rle function breaks a sequence into lengths and values
    length.repeats = rle(all_blocks)$lengths
    # Keep going if the max number of repeats is larger than 1
    keep.looking = max(length.repeats) > 1
  }  #end while
  
  # we repeat every category 20 times, followed by a long baseline trial. 
  task_order = c(); timing = c(); is_odd_ball = c(); iti = c();
  for (c in 1:length(all_blocks)){
    curr_task <- c(rep(all_blocks[c],n_stims_per_block))
    curr_odd_ball <- c(sample(c(rep(1,odd_ball_percent*n_stims_per_block),rep(0,(1-odd_ball_percent)*n_stims_per_block)))) 
    curr_timing <- c(rep(stim_duration,n_stims_per_block))
    curr_iti <- c(rep(iti_duration, n_stims_per_block-1),timing_baseline_block)
    task_order = c(task_order, curr_task)
    timing = c(timing, curr_timing)
    is_odd_ball = c(is_odd_ball, curr_odd_ball)
    iti = c(iti,curr_iti)
  }
  
  stim_path <- c()
  stimulus_bank <- loc_stims
  for (t in 1:length(task_order)){
    stim_path[t] <- sample(stimulus_bank[stimulus_bank$category==task_order[t],"path"],1)
    stimulus_bank <- stimulus_bank %>% subset(path != stim_path[t])
  }
  
  # randomize the images 
  localizer <- rbind(loc_practice_trials, 
                     data.frame(
                       PID = NaN,
                       practice = 0,
                       trial = NaN,
                       run = 1,
                       task = task_order,
                       stimulus_duration = timing,
                       iti = iti,
                       is_odd_ball = is_odd_ball,
                       path = stim_path,
                       response = NaN,
                       rt = NaN))
  
  write.csv(localizer, sprintf("../Task_sequences/Localizer/localizer_v%d.csv",i), row.names = FALSE)
  
}

# ==============================================================================
# Ratings
# ==============================================================================

for (i in 1:n_not_scanned_designs){
  Ratings <- exp_stims %>%
    subset(type == "original") %>% 
    slice(sample(1:n())) %>%
    dplyr::select(-c(type)) %>%
    rename(stimulus_index = index) %>%
    mutate(PID = NaN,
           trial = 1:n(),
           rt = NaN,
           rating = NaN) %>%
    rename(stimulus = stim) %>%
    relocate(c(PID, trial, stimulus_index, rt, rating))
  
  write.csv(Ratings, sprintf("../Task_sequences/Ratings/ratings_v%d.csv",i), row.names = FALSE)
}


# ==============================================================================
# Deliberation
# ==============================================================================

# create all possible combinations of the four categories 
pair_types <- data.frame(t(combn(categories,2)))

# repeat the same trial types 
trials <- pair_types %>% slice(rep(row_number(), n_reps_of_deliberation_pair_types)) %>%
  dplyr::rename(left_category = X1, right_category = X2)

# counterbalance position of category on the left (switch column position in second half of trials)
first_half <- trials[1:(nrow(trials)/2),c("left_category","right_category")]
second_half <- trials[1:(nrow(trials)/2),c("right_category","left_category")]
colnames(second_half) <- c("left_category","right_category")
trials <- rbind(first_half, second_half) 

# add pair id and practice
trials <- trials %>% mutate(PID = NaN,
                            pair_id = LETTERS[1:nrow(trials)], 
                            practice = 0,
                            scramble_screen_time = NaN,
                            scramble_instructed_side = NaN,
                            scramble_acc = NaN,
                            no_response = 0,
                            left_stimulus = NaN,
                            right_stimulus = NaN,
                            left_path = NaN,
                            right_path = NaN,
                            left_rating = NaN,
                            right_rating = NaN,
                            rt = NaN,
                            left_chosen = NaN,
                            chosen_stimulus = NaN,
                            unchosen_stimulus = NaN,
                            chosen_category = NaN,
                            unchosen_category = NaN,
                            chosen_path = NaN,
                            unchosen_path = NaN)

# add scramble trials 
trials <- trials %>% mutate(stim_type = "original") %>%
  rbind(trials %>% mutate(stim_type = "scramble"))

# add practice trials 
prac_del_original = practice_stims %>% subset(scramble==0 & phase == "Deliberation")
prac_del_scramble = practice_stims %>% subset(scramble==1 & phase == "Deliberation")

practice_trials = data.frame(
  PID = NaN,
  left_category = rep(NaN, 4),
  right_category = rep(NaN, 4),
  pair_id = c("prac1", "prac2", "prac3", "prac4"),
  practice = 1,
  scramble_screen_time = c(NaN, 2500, 1500, NaN),
  scramble_instructed_side = c(NaN, "left", "right", NaN),
  scramble_acc = NaN,
  no_response = 0,
  left_stimulus = NaN,
  right_stimulus = NaN,
  left_path = c(prac_del_original$path[1], prac_del_scramble$path[3], prac_del_scramble$path[1], prac_del_original$path[3]),
  right_path = c(prac_del_original$path[2], prac_del_scramble$path[4], prac_del_scramble$path[2], prac_del_original$path[4]),
  left_rating = NaN,
  right_rating = NaN,
  rt = NaN,
  left_chosen = NaN,
  chosen_stimulus = NaN,
  unchosen_stimulus = NaN,
  chosen_category = NaN,
  unchosen_category = NaN,
  chosen_path = NaN,
  unchosen_path = NaN,
  stim_type = c("original", "scramble", "scramble", "original"),
  iti = c(1500, 2500, 3000, 2000),
  block = 0,
  run = 0
)

for (i in 1:n_scanned_designs){
  
  deliberation_trials = data.frame()
  for (b in 1:n_deliberation_blocks){
    # add shuffled iti for each stimulus type
    original_trials = trials %>% subset(stim_type == "original") %>% mutate(iti = sample(timing_deliberation_iti[rep(1:nrow(timing_deliberation_iti), timing_deliberation_iti$reps),"sec"]))
    scramble_trials = trials %>% subset(stim_type == "scramble") %>% mutate(iti = sample(timing_deliberation_iti[rep(1:nrow(timing_deliberation_iti), timing_deliberation_iti$reps),"sec"]))
    # add scramble time for scramble trials (using average rt and sd from Exp1 and the Pilot study in Biderman and Shohamy (2021))
    # we sample from a truncated normal distribution, with a minimum of 50 ms seconds. 
    scramble_trials <-  mutate(scramble_trials, 
                                scramble_screen_time = case_when(b==1 ~ round(rtruncnorm(nrow(scramble_trials), a=0.05, b=max_scramble_time, mean=mean_del_block1, sd=sd_del_block1),1)*1000,
                                                                 b==2 ~ round(rtruncnorm(nrow(scramble_trials), a=0.05, b=max_scramble_time, mean=mean_del_block2, sd=sd_del_block2),1)*1000,
                                                                 b==3 ~ round(rtruncnorm(nrow(scramble_trials), a=0.05, b=max_scramble_time, mean=mean_del_block3, sd=sd_del_block3),1)*1000,
                                                                 b==4 ~ round(rtruncnorm(nrow(scramble_trials), a=0.05, b=max_scramble_time, mean=mean_del_block4, sd=sd_del_block4),1)*1000))
    
    scramble_trials$scramble_instructed_side = sample(rep(c("left","right"), each=nrow(scramble_trials)/2));
    # combine the two trial types, add block number and shuffle
    curr_trials = rbind(original_trials, scramble_trials) %>% 
      mutate(block = b) %>%
      slice(sample(1:n()))
    # add to all deliberation trials
    deliberation_trials <- rbind(deliberation_trials, curr_trials)
  }
  
  # add practice trials on top
  deliberation_trials <- deliberation_trials %>% mutate(run = rep(1:n_deliberation_runs,each=nrow(deliberation_trials)/2))
  deliberation_trials <- rbind(practice_trials, deliberation_trials)
  
  # reorder columns
  deliberation_trials <- deliberation_trials %>% dplyr::select("PID","pair_id","stim_type", "practice", "run", "block", "left_category", "right_category", "left_stimulus", "right_stimulus","left_path", "right_path", "left_rating","right_rating","iti", "scramble_screen_time", "scramble_instructed_side", "scramble_acc", "no_response","rt","left_chosen", "chosen_stimulus", "unchosen_stimulus", "chosen_category", "unchosen_category","chosen_path","unchosen_path") # change order of trials 
  deliberation_trials <- deliberation_trials %>% mutate(trial = 1:n()) %>% relocate(trial,.after="PID")
    
  
  # save the current design 
  write.csv(deliberation_trials, "../Task_sequences/Deliberation/deliberation.csv", row.names = FALSE)
}

# ==============================================================================
# Outcome learning
# ==============================================================================

# TODO: add gains and losses in js code with the constraint that categories that repeat more than once have gain and losses. 
# TODO: count repetitions of each stimulus id 

# add practice trials 
prac_outcome_learning = practice_stims %>% subset(phase == "Outcome_learning")
outcome_learning_practice <- data.frame(
  PID = NaN,
  practice = rep(1, 4),
  run= 0,
  block=0,
  trial = 1:4,
  item_id = c("prac1", "prac2", "prac2", "prac1"),
  stimulus = NaN,
  path = c(prac_outcome_learning$path[1],prac_outcome_learning$path[2], prac_outcome_learning$path[2], prac_outcome_learning$path[1]),
  outcome = c("gain", "no-gain", "no-gain", "gain"),
  iti = c(1500, 1000, 4000, 2000),
  isi = c(1000, 1500, 1000, 2000),
  response = NaN,
  response_acc = NaN,
  response_rt = NaN
)

# assign item place holder 
items <- LETTERS[1:n_deliberation_pairs]
outcomes <- data.frame(PID = NaN, practice=0, run=NaN, block=NaN, trial=NaN, item_id=items, stimulus=NaN, path=NaN, outcome=NaN, iti=NaN, isi=NaN, response = NaN, response_acc = NaN, response_rt = NaN)

for (i in 1:n_scanned_designs){
  
  outcome_learning_trials = c()
  for (j in 1:n_outcome_learning_rep){
    curr_rep <- outcomes %>%
      mutate(block = j) %>%
      dplyr::slice(sample(1:n())) # shuffle
    outcome_learning_trials <- rbind(outcome_learning_trials, curr_rep) 
  }
  
  # add practice trials 
  outcome_learning_trials <- outcome_learning_trials %>% dplyr::mutate(run = rep(1:n_outcome_learning_runs,each=nrow(outcome_learning_trials)/2))
  outcome_learning_trials <- rbind(outcome_learning_practice,outcome_learning_trials) %>%
    dplyr::mutate(trial = 1:n())
  
  # save the current design 
  write.csv(outcome_learning_trials, "../Task_sequences/Outcome_learning/outcome_learning.csv", row.names = FALSE)
}

# ==============================================================================
# Final decisions
# ==============================================================================

# create all possible combinations of gains and losses 
gains <- paste0(rep("gain", n_deliberation_pairs/2), 1:(n_deliberation_pairs/2))
no_gains <- paste0(rep("no_gain", n_deliberation_pairs/2), 1:(n_deliberation_pairs/2))
pairs <- data.frame(crossing(gains, no_gains))

# repeat the pairs for every choice condition and blocks
all_trials <- data.frame(
  PID = NaN,
  practice=0,
  trial = NaN,
  block = rep(1:n_fd_blocks, each=nrow(pairs)*2),
  run = rep(1:n_fd_runs, each=nrow(pairs)*2),
  left_pair_id = NaN,
  right_pair_id = NaN,
  left_stimulus = NaN,
  right_stimulus = NaN,
  left_category = NaN,
  right_category = NaN,
  left_path = NaN,
  right_path = NaN,
  no_response = 0,
  left_chosen = NaN,
  rt = NaN,
  higher_outcome_chosen = NaN,
  gain = rep(pairs$gains, n_fd_blocks*2),
  no_gain = rep(pairs$no_gains, n_fd_blocks*2),
  choice_type = rep(rep(c("chosen","unchosen"),each=nrow(pairs)),n_fd_blocks)
)

prac_stims_fd <- practice_stims %>% subset(phase == "Final_decisions") 

# randomize order of gain on the left, add iti within a block and trial type, and shuffle order of trials 
for (i in 1:n_scanned_designs){
  final_decisions_trials <- data.frame()
  for (b in 1:n_fd_blocks){
    # randomize order of gains for chosen and unchosen trials separately 
    gain_left <- c(sample(rep(c(0,1),nrow(pairs)/2)), sample(rep(c(0,1),nrow(pairs)/2)))
    curr_block <- all_trials %>% 
      subset(block==b) %>%
      mutate(gain_left = gain_left,
             left_item_id = ifelse(gain_left==1, gain, no_gains),
             right_item_id = ifelse(gain_left==1, no_gains, gain)) %>%
      relocate(gain_left, left_item_id, right_item_id, .after="run") %>%
      # add iti 
      mutate(iti = c(sample(timing_final_decisions_iti[rep(1:nrow(timing_final_decisions_iti), timing_final_decisions_iti$reps),"sec"]),
                     sample(timing_final_decisions_iti[rep(1:nrow(timing_final_decisions_iti), timing_final_decisions_iti$reps),"sec"]))) %>%
      # shuffle
      slice(sample(1:n())) %>%
      dplyr::select(-c(gain, no_gain)) 
    # add block to data frame
    final_decisions_trials <- rbind(final_decisions_trials,curr_block) 
  }
  
  # add practice trials 
  practice_fd_trials <- data.frame(
    PID = NaN,
    practice=1,
    trial = NaN,
    block = 0,
    run = 0,
    gain_left = NaN,
    left_item_id = NaN,
    right_item_id = NaN,
    left_pair_id = NaN,
    right_pair_id = NaN,
    left_stimulus = NaN,
    right_stimulus = NaN,
    left_category = NaN,
    right_category = NaN,
    left_path = c(prac_stims_fd$path[1], prac_stims_fd$path[2], prac_stims_fd$path[1], prac_stims_fd$path[4]),
    right_path = c(prac_stims_fd$path[4], prac_stims_fd$path[3], prac_stims_fd$path[2], prac_stims_fd$path[1]),
    no_response = 0,
    left_chosen = NaN,
    rt = NaN,
    higher_outcome_chosen = NaN,
    choice_type = NaN,
    iti = c(3000, 1500, 2000, 1000)
  )
  
  final_decisions_trials <- rbind(practice_fd_trials, final_decisions_trials) %>%
    mutate(trial = 1:n())
  
  # save the current design 
  write.csv(final_decisions_trials, "../Task_sequences/Final_decisions/final_decisions.csv", row.names = FALSE)
}

# ==============================================================================
# Memory test
# ==============================================================================

n_memory_reps = 2 # per run
n_memory_runs = 2

memory_prac_stims = practice_stims %>% subset(phase == "Memory")
memory_practice_trials = data.frame(
  PID = NaN,
  practice = 1,
  run = 0,
  block = 0,
  trial = NaN,
  pair_type = NaN,
  old_pair = NaN,
  chosen_left = NaN,
  pair_id = NaN,
  chosen_id = NaN,
  iti = c(1500, 1000, 3000),
  left_stimulus = NaN,
  right_stimulus = NaN,
  left_path = c(memory_prac_stims$path[1], memory_prac_stims$path[2], memory_prac_stims$path[3]),
  right_path = c(memory_prac_stims$path[2], memory_prac_stims$path[3], memory_prac_stims$path[1]),
  pair_response = NaN,
  pair_acc = NaN,
  pair_rt = NaN
)

for (i in 1:n_scanned_designs){
  
  memory_trials_tmp <- c()
  
  for (r in 1:n_memory_runs){
    for (rep in 1:n_memory_reps){
      curr_memory_trials <- data.frame(
        PID = NaN,
        practice = 0,
        run = NaN,
        block = NaN,
        trial = NaN,
        pair_type = rep(c("intact","recombined"),each=n_deliberation_pairs),
        old_pair = rep(c(1,0),each=n_deliberation_pairs),
        chosen_left = c(rep(NaN,n_deliberation_pairs),sample(rep(c(0,1), n_deliberation_pairs/2))), # only recombined pairs have shuffled location of the chosen item
        pair_id = c(LETTERS[1:n_deliberation_pairs],rep(NaN,n_deliberation_pairs)),
        chosen_id = rep(LETTERS[1:n_deliberation_pairs],2),
        iti = rep(sample(timing_memory_iti[rep(1:nrow(timing_memory_iti), timing_memory_iti$reps),"sec"]),each=2),
        left_stimulus = NaN,
        right_stimulus = NaN,
        left_path = NaN,
        right_path = NaN,
        pair_response = NaN,
        pair_acc = NaN,
        pair_rt = NaN) %>%
        slice(sample(1:n())) # shuffle order
      memory_trials_tmp <- rbind(memory_trials_tmp, curr_memory_trials)
    }
  }
  memory_trials_tmp <- memory_trials_tmp %>% mutate(run = rep(1:n_memory_runs,each=n()/n_memory_runs),
                                                    block = rep(1:(n_memory_runs*n_memory_reps), each=n()/(n_memory_runs*n_memory_reps)))
  memory_trials <- rbind(memory_practice_trials,memory_trials_tmp) %>%
    mutate(trial = 1:n())
  
  # save the current design 
  write.csv(memory_trials, "../Task_sequences/Memory/memory_%d.csv", row.names = FALSE)
}

# ==============================================================================
# Final ratings
# ==============================================================================

for (i in 1:n_not_scanned_designs){
  final_ratings <- exp_stims %>%
    slice(sample(1:n())) %>%
    rename(stimulus_index = index) %>%
    mutate(PID = NaN,
           trial = 1:n(),
           rt = NaN,
           old_rating = NaN,
           new_rating = NaN) %>%
    rename(stimulus = stim) %>%
    relocate(c(PID, trial, rt, old_rating, new_rating))
  
  write.csv(final_ratings, sprintf("../Task_sequences/Final_ratings/final_ratings_v%d.csv",i), row.names = FALSE)
}

# ==============================================================================
# Outcome estimation
# ==============================================================================

n_outcome_estimation_reps = 2 # per run
n_outcome_estimation_runs = 2

outcome_etimation_prac_stims = practice_stims %>% subset(phase == "Outcome_estimation")
outcome_estimation_practice_trials = data.frame(
  PID = NaN,
  practice = 1,
  run = 0,
  block = NaN,
  trial = NaN,
  iti = c(1500, 1000, 3000),
  pair_id = NaN,
  choice_type = NaN,
  reward_type = NaN,
  stimulus_id = NaN,
  path = c(outcome_etimation_prac_stims$path[1], outcome_etimation_prac_stims$path[2], outcome_etimation_prac_stims$path[3]),
  outcome_response = NaN,
  outcome_rt = NaN,
  outcome_acc = NaN,
  confidence_response = NaN,
  confidence_rt = NaN)

for (i in 1:n_scanned_designs){
  
  outcome_estimation_trials_tmp <- c()
  
  for (r in 1:n_outcome_estimation_runs){
    for (rep in 1:n_outcome_estimation_reps){
      curr_outcome_estimation_trials <- data.frame(
        PID = NaN,
        practice = 0,
        run = NaN,
        block = NaN,
        trial = NaN,
        iti = rep(sample(timing_outcome_estimation_iti[rep(1:nrow(timing_outcome_estimation_iti), timing_outcome_estimation_iti$reps),"sec"]),each=2),
        pair_id = LETTERS[1:n_deliberation_pairs],
        choice_type = rep(c("chosen","unchosen"), each=n_deliberation_pairs),
        reward_type = NaN,
        stimulus_id = NaN,
        path = NaN,
        outcome_response = NaN,
        outcome_rt = NaN,
        outcome_acc = NaN,
        confidence_response = NaN,
        confidence_rt = NaN) %>%
        slice(sample(1:n())) # shuffle order
      outcome_estimation_trials_tmp <- rbind(outcome_estimation_trials_tmp, curr_outcome_estimation_trials)
    }
  }
  
  outcome_estimation_trials_tmp <- outcome_estimation_trials_tmp %>% mutate(run = rep(1:n_outcome_estimation_runs,each=n()/n_outcome_estimation_runs),
                                                                            block = rep(1:(n_outcome_estimation_runs*n_outcome_estimation_reps), each=n()/(n_outcome_estimation_runs*n_outcome_estimation_reps)))
  
  outcome_estimation_trials <- rbind(outcome_estimation_practice_trials,outcome_estimation_trials_tmp) %>%
    mutate(trial = 1:n())
  
  write.csv(outcome_estimation_trials, "../Task_sequences/Outcome_estimation/outcome_estimation_%d.csv", row.names = FALSE)
}




