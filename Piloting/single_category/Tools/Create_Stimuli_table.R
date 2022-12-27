## Create a table of stimuli
library(stringr)
folder = "../Stimuli/Experimental_stims/"
categories = c("People", "Objects", "Scenes")
files_data = c()
for (i in 1:length(categories)){
  curr_files_data = list.files(paste0(folder,"/",categories[i]), all.files = TRUE ,pattern="*.jpg")
  files_data = c(files_data,curr_files_data)
}
Stimuli <- "["
for (i in 1:length(files_data)){
  if (i != length(files_data)){
    Stimuli <- sprintf("%s['%s','%s',%d],",Stimuli,str_remove(gsub('[0-9]+', '', files_data[i]),".jpg"),files_data[i],i-1)
  } else {
    Stimuli <- sprintf("%s['%s','%s',%d]]",Stimuli,str_remove(gsub('[0-9]+', '', files_data[i]),".jpg"),files_data[i],i-1)
  }
}