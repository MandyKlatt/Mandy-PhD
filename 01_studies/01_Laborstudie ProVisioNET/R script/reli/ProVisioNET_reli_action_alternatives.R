### ProVisioNET pilot data 
#### intercoder reli expertise data 02_02


# install needed packages
library(needs)
needs(tidyverse,
      psych,
      moments,
      sjPlot,
      irr,
      readxl,
      plyr)



# suppress "summarize" info. 
# if this line is ommitted, each table using the summarize function will be accompanied with a warning from the console
options(dplyr.summarise.inform = FALSE)


################## RATER 1 ################

# read in data from rater1 while specifying locale allows to set "," 
r1 <-read_excel ("./data/coding_handlungsalternativen_JG.xlsx", col_names = TRUE)


################## RATER 2 ################

# read in data from rater1 while specifying locale allows to set "," 
r2 <-read_excel ("./data/coding_handlungsalternativen_MK.xlsx", col_names = TRUE)

r4 <-read_excel ("./data/coding_handlungsalternativen_JG_MK_nur_ratings.xlsx", col_names = TRUE)

################## DATA WRANGLING video 01 ################

# filter relevant rows and select relevant columns
r1 <- r1 %>% select(number) %>% 
  na.omit(r1)
  
r2 <- r2 %>% select(number) %>% 
  na.omit(r2)

# # reshape data frame in long format 
# r1_long <- r1 %>% 
#   pivot_longer(!ID, 
#                names_to = "Event", 
#                values_to = "Value")
# 
# r2_long <- r2 %>% 
#   pivot_longer(!ID, 
#                names_to = "Event", 
#                values_to = "Value")

r1$number <- as.numeric(r1$number)
r2$number <- as.numeric(r2$number)


r3 <- bind_cols(r1$number, r2$number)

#################### Percentage Agreement ##############################
agree(r3, tolerance = 0)

r4 <- subset(r4, select = c(code1, code2))
agree(r4, tolerance = 0)

#################### CohenKappa ##############################
kappa <- psych::cohen.kappa(x = as.matrix(r3))
kappa <- psych::cohen.kappa(x = as.matrix(r4))


################## ICC ###########################
ICC(r3)





