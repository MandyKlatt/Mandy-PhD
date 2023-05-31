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


# read in data for numbers of action alternatives
df_number <- read_excel ("./data/coding_handlungsalternativen_JG_MK_number.xlsx", col_names = TRUE) %>% 
  select(rater1, rater2)

# read in data for rating of action alternatives
df_rating <- read_excel ("./data/coding_handlungsalternativen_JG_MK_ratings.xlsx", col_names = TRUE) %>% 
  mutate(rater1 = as.numeric(case_when(rater1 > 3 ~ '0',
                                       rater1 <= 3 ~ '1',
                                       TRUE ~ 'F')
                             ),
         rater2 = as.numeric(case_when(rater2 > 3 ~ '0',
                                       rater2 <= 3 ~ '1',
                                       TRUE ~ 'F')
                             )
         ) %>%
  select(rater1, rater2)

#################### Percentage Agreement ##############################

# number of alternatives
agree(df_number, tolerance = 1)

# rating of alternatives
agree(df_rating, tolerance = 0)


#################### CohenKappa ##############################

# number of alternatives
psych::cohen.kappa(x = as.matrix(df_number))

# rating of alternatives
psych::cohen.kappa(x = as.matrix(df_rating))


