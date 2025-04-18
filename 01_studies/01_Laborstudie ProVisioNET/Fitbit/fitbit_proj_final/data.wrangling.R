rm(list = ls())

##### 1. Test Data #####


df_test <- read_tsv(file = "data/Acc_Test_Data.tsv") %>% 
  clean_names() %>%
  transmute(time = as.numeric(milliseconds(recording_timestamp_ms)
                              ),
  name = participant_name,
  acc_x = as.numeric(str_replace(accelerometer_x_m_s2,
                                 pattern = ",",
                                 replacement = "."
                                 )
                     ),
  acc_z = as.numeric(str_replace(accelerometer_z_m_s2,
                                 pattern = ",",
                                 replacement = "."
                                 )
                     )
  )

# First step: create tibbles for each individual.
for (i in unique(df_test$name)) {
  
  df <- filter(df_test,
               name == i
               )
  
  assign(value = df,
         x = paste0("df_", i)
         )
  }

# For each of the tibbles, create two vectors (x and z) 
# for meters for each time period.

for (i in unique(df_test$name)) {
  
  df <- get(paste0("df_", i))
  
  # Vectors x and z with length equal to time periods 
  # present in each tibble; initial with 0 meters for each   # time period. 
  
  x <- rep(0, length(get(paste0("df_", i))$time))
  z <- rep(0, length(get(paste0("df_", i))$time))
  
  # For each time period, calculate the meters walked in 
  # two dimensions (x and z), given the initial velocity 
  # for each period is 0 m/s. First value stays 0 for
  # convenience.
  # See formula 1.
  
  for (j in 2:length(get(paste0("df_", i))$time)) {
    
    x[[j]] <- 0.5 * get(paste0("df_", i))$acc_x[j] * (get(paste0("df_", i))$time[j] - get(paste0("df_", i))$time[j-1])^2
    
    z[[j]] <- 0.5 * get(paste0("df_", i))$acc_z[j] * (get(paste0("df_", i))$time[j] - get(paste0("df_", i))$time[j-1])^2
    
  }
  
  # Save both vectors in new tibble.
  
  df <- tibble(meter_x = x,
               meter_z = z,
               ID = paste0(i))
  assign(x = paste0("df_meter_", i),
         value = df)
  }

# Combine all tibbles containing distance walked per 
# time period.
# Calculate sum of absolute distance walked for each 
# participant and dimension.
# See formula 2.

labs <- unique(df_test$name)

df_test$name <- factor(df_test$name,
                       levels = labs,
                       labels = c("Standing Still",
                                  "Standing Still, Head Movement",
                                  "Walking Ten Meters",
                                  "Walking Ten Meters, Turning Around",
                                  "Walking More, Random",
                                  "Walking Less, Random",
                                  "Stairs",
                                  "Elevator"))

saveRDS(object = df_test,
        file = "final_data/meter_test_time.rds")

df_meter_all <- 
  mget(ls(pattern = "meter")) %>%
  bind_rows() %>% 
  group_by(ID) %>%
  summarise(meter_x = sum(abs(meter_x)),
            meter_z = sum(abs(meter_z))
            )

# Calculate walked distance in two-dimensional space.
# See formula 3.

df_meter_all$ges <- sqrt(df_meter_all$meter_x^2 + df_meter_all$meter_z^2)

saveRDS(object = df_meter_all,
        file = "final_data/meter_test.rds"
        )

rm(list = ls())

##### 2. Acceleration Data (real) #####

# df_acc <- read_tsv(file = "Data/ProVisioNET.tsv") %>% 
#   clean_names() %>% 
#   transmute(ID = str_replace(str_sub(participant_name,
#                                      start = 19,
#                                      end = 21
#                                      ),
#                              pattern = "_",
#                              replacement = ""
#                              ),
#             acc_x = as.numeric(str_replace(accelerometer_x,
#                                            pattern = ",",
#                                            replacement = "."
#                                            )
#                                ),
#             acc_z = as.numeric(str_replace(accelerometer_z,
#                                            pattern = ",",
#                                            replacement = "."
#                                            )
#                                ),
#             time = as.numeric(milliseconds(recording_timestamp
#                                            )
#                               ),
#             sensor = sensor
#             ) %>% 
#   filter(as.numeric(ID) > 100) %>%
#   select(!contains("gyro")) %>%
#   na.omit() %>%
#   select(!"sensor")
# 
# # First step: create tibbles for each individual.
# for (i in unique(df_acc$ID)) {
#   
#   df <- filter(df_acc,
#                ID == i
#   )
#   
#   assign(value = df,
#          x = paste0("df_", i)
#   )
#   
# }
# 
# # For each of the tibbles, create two vectors (x and z) 
# # for meters for each time period.
# 
# for (i in unique(df_acc$ID)) {
#   
#   df <- get(paste0("df_", i))
#   
#   # Vectors x and z with length equal to time periods 
#   # present in each tibble; initial with 0 meters for each   # time period. 
#   
#   x <- rep(0, length(get(paste0("df_", i))$time))
#   z <- rep(0, length(get(paste0("df_", i))$time))
#   
#   # For each time period, calculate the meters walked in 
#   # two dimensions (x and z), given the initial velocity 
#   # for each period is 0 m/s. First value stays 0 for
#   # convenience.
#   # See formula 1.
#   
#   for (j in 2:length(get(paste0("df_", i))$time)) {
#     
#     x[[j]] <- 0.5 * get(paste0("df_", i))$acc_x[j] * (get(paste0("df_", i))$time[j] - get(paste0("df_", i))$time[j-1])^2
#     
#     z[[j]] <- 0.5 * get(paste0("df_", i))$acc_z[j] * (get(paste0("df_", i))$time[j] - get(paste0("df_", i))$time[j-1])^2
#     
#   }
#   
#   # Save both vectors in new tibble.
#   
#   df <- tibble(meter_x = x,
#                meter_z = z,
#                ID = paste0(i))
#   assign(x = paste0("df_meter_", i),
#          value = df)
#   
# }
# 
# # Combine all tibbles containing distance walked per 
# # time period.
# # Calculate sum of absolute distance walked for each 
# # participant and dimension.
# # See formula 2.
# 
# 
# 
# df_meter_all <- 
#   mget(ls(pattern = "meter")) %>%
#   bind_rows() %>% 
#   group_by(ID) %>%
#   summarise(meter_x = sum(abs(meter_x)),
#             meter_z = sum(abs(meter_z)))
# 
# # Calculate walked distance in two-dimensional space.
# # See formula 3.
# 
# df_meter_all$ges <- sqrt(df_meter_all$meter_x^2 + df_meter_all$meter_z^2)
# 
# saveRDS(object = df_meter_all,
#         file = "final_data/meter_real.rds")
# 
# rm(list = ls())
# 
# ##### 3. Pupil Dialation ####
# 
# df_dil <- read_rds(file = "Data/df.rds")
# 
# df_dil$date_time <- 
#   make_datetime(hour = hour(df_dil$start_time),
#                 min = minute(df_dil$start_time),
#                 sec = second(df_dil$start_time)
#                 ) + 
#   seconds(as.numeric(milliseconds(df_dil$time)
#                      )
#           )
# 
# df_dil$seconds <- floor(second(df_dil$date_time))
# df_dil$minutes <- minute(df_dil$date_time)
# df_dil$hours <- hour(df_dil$date_time)
# 
# df_dil_aggr <- 
#   df_dil %>% 
#   group_by(hours, minutes, seconds, ID) %>%
#   summarise(dil_left = mean(left),
#             dil_right = mean(right)
#             ) %>%
#   ungroup() %>%
#   mutate(Time = hms::hms(hours = hours,
#                     minutes = minutes,
#                     seconds = seconds
#                     )
#          ) %>%
#   select(dil_left,
#          dil_right,
#          Time,
#          ID
#          )
# 
# # df_dil_aggr$time <- 
# # hms(hours = df_dil_aggr$hours,
# #     minutes = df_dil_aggr$minutes,
# #     seconds = df_dil_aggr$seconds)
# # 
# 
# saveRDS(object = df_dil,
#         file = "final_data/dilation.rds")
# 
# saveRDS(object = df_dil_aggr,
#         file = "final_data/dilation_aggr.rds"
#         )
# 
# rm(list = ls())

##### 4. Heart Rate ####

part <- read_xlsx(path = "Data/Heart Rate.xlsx")

#...............................................................................
#                                                                              .
#  Enter the missing cases in Heart Rate table below.                                       .
#                                                                              .
#...............................................................................

part <- part %>% 
  filter(!`ID participant`%in% c("126", 
                                 "132", 
                                 "134")
         )
  
part <- part$`ID participant`

for (i in part) {
  nam <- paste("m", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("Data/", i, "_m.csv", 
                                       sep = ""
                                       )
                          )
         ) 
}

for (i in part) {
  nam <- paste("a", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("Data/", i, "_a.csv", 
                                       sep = ""
                                       )
                          )
         ) 
}

for (i in part) {
  nam <- paste("p", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("Data/", i, "_p.csv", 
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
        file = "final_data/heart_all.rds")

saveRDS(object = df_aggr,
        file = "final_data/heart_aggr.rds")

saveRDS(object = df_all_min,
        file = "final_data/heart_min.rds")

rm(list = ls())

##### 5. Steps ####

df_steps <- read_xlsx(path = "Data/Heart Rate.xlsx")

df_steps <- df_steps %>%
  transmute(ID = as.numeric(as.character(df_steps$`ID participant`)),
            Steps = Diff
            )

df_steps$type <- ifelse(df_steps$ID < 200, 
                        "Novice",
                        "Expert"
                        )
df_heart_aggr <- read_rds(file = "final_data/heart_aggr.rds")

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
        file = "final_data/steps.rds")

rm(list = ls())

##### 6. Heart Rate and Pupil Dilation ####


####
part <- read_xlsx(path = "Data/Heart Rate.xlsx")

#...............................................................................
#                                                                              .
#  Enter the missing cases in Heart Rate table below.                                       .
#                                                                              .
#...............................................................................

part <- part %>% 
  filter(!`ID participant`%in% c("126", 
                                 "132", 
                                 "134")
         )

part <- part$`ID participant`

for (i in part) {
  nam <- paste("m", i, sep = "")
  assign(x = nam, 
         value = read_csv(file = paste("Data/", i, "_m.csv", 
                                       sep = ""
         )
         )
  ) 
}

for (i in part) {
  nam <- paste0("m", i)
  `Heart Rate` <- get(nam)$`Heart Rate`
  time <- get(nam)$Time 
  df <- tibble(Time = time,
               `Heart Rate` = `Heart Rate`,
               ID = i
  )
  assign(paste0("m", i), df)
}

rm(part)
rm(nam)
rm(i)
rm(time)
rm(`Heart Rate`)

df <- 
  mget(ls(pattern = "m")) %>%
  bind_rows()

rm(list = ls(pattern = "m"))

df_dil_aggr <- read_rds(file = "final_data/dilation_aggr.rds")

df_dil_aggr$ID <- as.numeric(df_dil_aggr$ID)

df_hr_dil <- left_join(df, df_dil_aggr,
                       by = c("ID","Time")
                       )

df_hr_dil$type = as_factor(ifelse(df_hr_dil$ID < 200,
                                  "Novice",
                                  "Expert"
                                  )
                           )

df_hr_dil <- 
  df_hr_dil %>%
  group_by(ID) %>%
  summarise(Time = Time,
            heart_rate_z = scale(`Heart Rate`),
            dil_left_z = scale(dil_left),
            dil_right_z = scale(dil_right),
            type = type)

saveRDS(object = df_hr_dil,
        file = "final_data/heart_dilation.rds")


rm(list = ls())


#### Next Step: Std. pupil dialation per Participant!
