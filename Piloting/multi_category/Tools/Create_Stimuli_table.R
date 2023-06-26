
library(dplyr)

# ==============================================================================
# experimental stims
# ==============================================================================

library(stringr)
folder = "../Stimuli/Experimental_stims/"
categories = c("outdoor_scenes", "faces", "body_parts", "objects")
file_names = c()
category_names = c()
for (i in 1:length(categories)){
  curr_file_names = list.files(paste0(folder,"/",categories[i]), all.files = TRUE ,pattern="*.jpg")
  file_names = c(file_names,curr_file_names)
  curr_category_names = rep(categories[i],length(curr_file_names))
  category_names = c(category_names,curr_category_names)
}
original_stims <- data.frame()
for (i in 1:length(file_names)){
  original_stims[i,"index"] <- i-1
  original_stims[i,"type"] <- "original"
  original_stims[i,"category"] <- category_names[i]
  original_stims[i,"stim"] <- file_names[i]
  original_stims[i,"path"] <- paste0("Stimuli/Experimental_stims/",category_names[i],"/",file_names[i])
}
# add scramble 
scramble_stims <- original_stims
for (i in 1:length(file_names)){
  scramble_stims[i,"stim"] <- paste0("scrambled_",file_names[i])
  scramble_stims[i,"path"] <- paste0("Stimuli/Experimental_stims/scramble/",category_names[i],"/scrambled_",file_names[i])
  scramble_stims[i,"index"] <- length(file_names) + i - 1
  scramble_stims[i,"type"] <- "scramble"
}

exp_stims <- rbind(original_stims, scramble_stims)

write.csv(exp_stims, "../Stimuli/Stim_csv/experimental_stimuli.csv")

# ==============================================================================
# instructions stims
# ==============================================================================

instructions_files <- as.data.frame(list.files("../Stimuli/Instructions/mturk", all.files = TRUE ,pattern="*.png"))
instructions <- data.frame(scanner = "scanner",instructions = instructions_files) %>%
  rbind(data.frame(scanner = "mturk",instructions = instructions_files))
colnames(instructions) <- c("scanner", "instructions")
instructions <- instructions %>%
  mutate(path = paste0("Stimuli/Instructions/",scanner,"/",instructions))

write.csv(instructions, "../Stimuli/Stim_csv/instructions_stimuli.csv")

# ==============================================================================
# localizer stims
# ==============================================================================

library(stringr)
folder = "../Stimuli/Experimental_stims/localizer/"
categories = c("faces", "body_parts", "objects", "outdoor_scenes")
file_names = c()
category_names = c()
for (i in 1:length(categories)){
  curr_file_names = list.files(paste0(folder,"/",categories[i]), all.files = TRUE ,pattern="*.jpg")
  file_names = c(file_names,curr_file_names)
  curr_category_names = rep(categories[i],length(curr_file_names))
  category_names = c(category_names,curr_category_names)
}
loc_stims <- data.frame()
for (i in 1:length(file_names)){
  loc_stims[i,"index"] <- i-1
  loc_stims[i,"category"] <- category_names[i]
  loc_stims[i,"stim"] <- file_names[i]
  loc_stims[i,"path"] <- paste0("Stimuli/Experimental_stims/localizer/",category_names[i],"/",file_names[i])
}

write.csv(loc_stims, "../Stimuli/Stim_csv/localizer_stimuli.csv")

# ==============================================================================
# practice stims
# ==============================================================================

library(stringr)
prac_file_names = list.files("../Stimuli/Practice_stims", all.files = TRUE ,pattern="*.jpg", recursive=TRUE)
practice_stims <- data.frame(
  index = 1:length(prac_file_names),
  phase = sub("\\/.*", "", prac_file_names),
  scramble = as.numeric(str_detect(prac_file_names,"scramble")),
  category = "practice",
  path = prac_file_names) %>%
  mutate(path = paste0("Stimuli/Practice_stims/",path))

write.csv(practice_stims, "../Stimuli/Stim_csv/practice_stimuli.csv")


