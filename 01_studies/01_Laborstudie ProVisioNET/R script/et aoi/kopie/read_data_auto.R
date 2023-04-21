### ProVisioNET Eyetracking AOI Coding Data 

library(needs)
needs(tidyverse,
      psych,
      moments,
      sjPlot,
      DescTools,
      irr,
      readxl, 
      gridExtra)

# suppress "summarize" info. 
# if this line is ommitted, each table using the summarize function will be accompanied with a warning from the console
options(dplyr.summarise.inform = FALSE)

# Return a character vector with names of .tsv data

file_names <- list.files(path = "data",
                         pattern = "interval_complete.tsv")

# Read every object names in file_names and save it as a tibble

for (i in file_names) {

    work_data <- 
    read_tsv(file = paste0("data/", i),
<<<<<<< Updated upstream
                          locale = locale(decimal_mark = ","))
=======
                          locale = locale(decimal_mark = ",")) %>% 
      select(!"Minimum_amplitude_of_saccades")
>>>>>>> Stashed changes
  
  assign(value = work_data,
         x = str_remove(paste0("tib_", i),
                        pattern = ".tsv"
                        )
         )
  
}

# Bind every tibble that contains "interval_complete" to a new tiblle

df_aoi <- 
  mget(ls(pattern = "interval_complete")) %>%
  bind_rows()

rm(list = ls(pattern = "^tib_ProVisio"))
rm(work_data)
rm(file_names)
rm(i)
