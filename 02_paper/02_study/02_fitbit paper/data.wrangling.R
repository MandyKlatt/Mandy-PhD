
##### I. Heart Rate ####

part <- read_xlsx(path = "data/Heart Rate.xlsx")

#...............................................................................
#                                                                              .
#  Enter the missing cases in Heart Rate table below.                                       .
#                                                                              .
#...............................................................................

part <- part %>% 
  filter(!`ID participant`%in% c("126", 
                                 "132",
                                 "220",
                                 "221")
         )
  
part <- part$`ID participant`

for (i in part) {
  nam <- paste("m", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("data/", i, "_m.csv", 
                                       sep = ""
                                       )
                          )
         ) 
}

for (i in part) {
  nam <- paste("a", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("data/", i, "_a.csv", 
                                       sep = ""
                                       )
                          )
         ) 
}

for (i in part) {
  nam <- paste("p", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("data/", i, "_p.csv", 
                                       sep = ""
                                       )
                          )
         ) 
}

# Change to seconds

for (i in part) {
  nam <- paste0("m", i)
  value <- get(nam)$Time - min(get(nam)$Time)
  `Heart Rate` <- get(nam)$`Heart Rate`
  df <- tibble(Time = as.numeric(value),
               `Heart Rate` = `Heart Rate`,
               ID = i
               )
  assign(paste0("m", i), df)
}

for (i in part) {
  nam <- paste0("a", i)
  value <- get(nam)$Time - min(get(nam)$Time)
  `Heart Rate` <- get(nam)$`Heart Rate`
  df <- tibble(Time = as.numeric(value),
               `Heart Rate` = `Heart Rate`,
               ID = i
               )
  assign(paste0("a", i), df)
}

for (i in part) {
  nam <- paste0("p", i)
  value <- get(nam)$Time - min(get(nam)$Time)
  `Heart Rate` <- get(nam)$`Heart Rate`
  df <- tibble(Time = as.numeric(value),
               `Heart Rate` = `Heart Rate`,
               ID = i
               )
  assign(paste0("p", i), df)
}

# Join Data Frames

rm(part)
rm(nam)
rm(`Heart Rate`)
rm(i)
rm(value)
rm(df)

df_m <- 
  mget(ls(pattern = "m")) %>%
  bind_rows()

df_p <- 
  mget(ls(pattern = "p")) %>%
  bind_rows()

df_a <- 
  mget(ls(pattern = "a")) %>%
  bind_rows()

df_a$Type <- "Post"
df_m$Type <- "Main"
df_p$Type <- "Pre"

df_all <- bind_rows(list(df_a,df_p, df_m))

df_all <- df_all %>%
  mutate(Type = factor(Type,
                       levels = c("Pre","Main","Post")),
         ID = as_factor(ID)
  )

df_all$status <- if_else(as.numeric(as.character(df_all$ID)) < 200,
                         "Novice",
                         "Expert")

df_aggr <- df_all %>%
  group_by(Type, ID, status) %>%
  summarise(mean = mean(`Heart Rate`, na.rm = TRUE))

df_all_min <- df_all %>%
  group_by(ID) %>%
  summarise(min = max(Time))

# df_all df_aggr df_all_min

saveRDS(object = df_all,
        file = "data/heart_all.rds")

saveRDS(object = df_aggr,
        file = "data/heart_aggr.rds")

saveRDS(object = df_all_min,
        file = "data/heart_min.rds")

rm(list = ls())

##### II. Steps ####

df_steps <- read_xlsx(path = "data/Heart Rate.xlsx")


df_steps <- df_steps %>%
  transmute(ID = as.numeric(as.character(df_steps$`ID participant`)),
            Steps = Diff
            )

df_steps$type <- ifelse(df_steps$ID < 200, 
                        "Novice",
                        "Expert"
                        )
df_heart_aggr <- read_rds(file = "data/heart_aggr.rds")

df_aggr_main <- 
  df_heart_aggr %>% 
  mutate(ID = as.numeric(as.character(ID))) %>%
  ungroup() %>%
  filter(Type == "Main") %>%
  select(ID, mean)

df_steps <- full_join(x = df_aggr_main,
                      y = df_steps,
                      key = ID)

# df_steps$mean <- scale(df_steps$mean)
# df_steps$Steps <- scale(df_steps$Steps)

saveRDS(object = df_steps,
        file = "data/steps.rds")

rm(list = ls())

