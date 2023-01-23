### ProVisioNET pilot data 
#### intercoder reli expertise data 02_02


# install needed packages
library(needs)
needs(tidyverse,
      psych,
      moments,
      sjPlot,
      irr,
      readxl)



# suppress "summarize" info. 
# if this line is ommitted, each table using the summarize function will be accompanied with a warning from the console
options(dplyr.summarise.inform = FALSE)


################## RATER 1 ################

# read in data from rater1 while specifying locale allows to set "," 
r1 <-read_excel ("./data/coding_expertise_reaction_MN.xlsx", col_names = TRUE)


################## RATER 2 ################

# read in data from rater1 while specifying locale allows to set "," 
r2 <-read_excel ("./data/coding_expertise_reaction_NT.xlsx", col_names = TRUE)


################## DATA WRANGLING video 01 ################

# filter relevant rows and select relevant columns 
r1 <- r1 %>% filter(`VP ID` == "101")

r2 <- r2 %>% filter(`VP ID` == "101")


# reshape data frame in long format 
r1_long <- r1 %>% 
  pivot_longer(!`VP ID`, 
               names_to = "Event", 
               values_to = "Value")

r2_long <- r2 %>% 
  pivot_longer(!`VP ID`, 
               names_to = "Event", 
               values_to = "Value")

r1_long$Value <- as.numeric(r1_long$Value)

r2_long$Value <- as.numeric(r2_long$Value)


# merge two data frames vertically
r3 <- bind_cols(r1_long$Value, r2_long$Value) %>%
  rename(rating1 = ...1, 
         rating2 = ...2)

#################### Percentage Agreement ##############################
agree(r3)

#################### CohenKappa ##############################
kappa <- psych::cohen.kappa(x = as.matrix(r3))








