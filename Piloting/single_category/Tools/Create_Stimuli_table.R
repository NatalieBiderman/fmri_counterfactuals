## Create a table of stimuli
library(stringr)
folder = "../Stimuli/Experimental_stims/"
categories = c("People", "Objects", "Scenes")
file_names = c()
category_names = c()
for (i in 1:length(categories)){
  curr_file_names = list.files(paste0(folder,"/",categories[i]), all.files = TRUE ,pattern="*.jpg")
  file_names = c(file_names,curr_file_names)
  curr_category_names = rep(categories[i],length(curr_file_names))
  category_names = c(category_names,curr_category_names)
}
Stimuli <- "["
for (i in 1:length(file_names)){
  if (i != length(file_names)){
    Stimuli <- sprintf("%s['%s','%s',%d],",Stimuli,category_names[i],file_names[i],i-1)
  } else {
    Stimuli <- sprintf("%s['%s','%s',%d]]",Stimuli,category_names[i],file_names[i],i-1)
  }
}