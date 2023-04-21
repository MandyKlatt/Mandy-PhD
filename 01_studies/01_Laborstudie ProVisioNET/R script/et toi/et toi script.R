### ProVisioNET Eyetracking TOI Coding Data 

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


<<<<<<< Updated upstream
# Return a character vector with names of .tsv data in data folder

file_names <- list.files(path = "data",
                         pattern = "interval_complete.tsv")

# Read every object names in file_names and save it as a tibble

for (i in file_names) {
  
  work_data <- 
    read_tsv(file = paste0("data/", i),
             locale = locale(decimal_mark = ","))
  
  assign(value = work_data,
         x = str_remove(paste0("tib_", i),
                        pattern = ".tsv"
         )
  )
  
}

# Bind every tibble that contains "interval_complete" to a new tiblle

df_toi <- 
  mget(ls(pattern = "interval_complete")) %>%
  bind_rows()

# Remove temporary data for a cleaner workspace

rm(list = ls(pattern = "^tib_ProVisio"))
rm(work_data)
rm(file_names)
rm(i)
=======
# read in expert data
expert_toi1 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_202_203_204_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

expert_toi2 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_205_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

expert_toi3 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_206_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

expert_toi4 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_207_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

# combine the expert df
expert_toi <- rbind(expert_toi1, 
                    expert_toi2, 
                    expert_toi3, 
                    expert_toi4)



# read in novice data
novice_toi1 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_101_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi2 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_102_103_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi3 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_104_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi4 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_105_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi5 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_106_107_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))


novice_toi6 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_108-111_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi7 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_112_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi8 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_113-115_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi9 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_116_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_toi10 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_117_interval_complete.tsv",
                         locale = locale(decimal_mark = ","))


# combine the novice df
novice_toi <- rbind(novice_toi1, 
                    novice_toi2, 
                    novice_toi3, 
                    novice_toi4, 
                    novice_toi5, 
                    novice_toi6, 
                    novice_toi7,
                    novice_toi8,
                    novice_toi9,
                    novice_toi10)

>>>>>>> Stashed changes

# combine the both df
toi <- rbind(novice_toi,
                expert_toi)

############### TIME TO FIRST REACTION ####################

# filter relevant rows only for time to first reaction
toi_react <- toi %>% filter (TOI == "Chatting_with_neighbour"|
                                    TOI == "Clicking_pen"| 
                                    TOI == "Drawing"|
                                    TOI == "Drumming_with_hands"| 
                                    TOI == "Head_on_table"| 
                                    TOI == "Heckling"|
                                    TOI == "Looking_at_phone" |
                                    TOI == "Snipping_with_fingers"|
                                    TOI == "Whispering")


# select relevant columns only for time to first event
toi_react <- toi_react %>% 
  select(Participant, Group, TOI,
         Time_to_first_Event.Reaction_chatting,
         Time_to_first_Event.Reaction_clicking,
         Time_to_first_Event.Reaction_drawing,
         Time_to_first_Event.Reaction_drumming,
         Time_to_first_Event.Reaction_head,
         Time_to_first_Event.Reaction_heckling,
         Time_to_first_Event.Reaction_phone,
         Time_to_first_Event.Reaction_snipping,
         Time_to_first_Event.Reaction_whispering)


# # compute "row-wise" sum across multiple columns but instead of typing column names, use tidy selection syntax
# toi_react <- toi_react %>%
#   rowwise() %>%
#   mutate(Time_to_first_Reaction = sum(c_across(cols = contains("Time_to")),
#                                       na.rm = T)) %>%
#   ungroup() %>%
#   select(Group, TOI, Time_to_first_Reaction)


# changing format with pivot_longer()
toi_react <-
  toi_react %>%
  pivot_longer(cols = contains("Time_to"),
               names_to = "Time_to_first_reaction",
               values_to = "Time_to_first_Reaction") %>%
  na.omit() %>% 
  select(Group, TOI, Participant, Time_to_first_Reaction)


# changing milliseconds into seconds
toi_react$`Time to first reaction in seconds`<- round(toi_react$Time_to_first_Reaction/1000,
                                                    digits = 2)


# plotting time to first reaction for groups
react_group_plot <- 
  ggplot(data = toi_react,
         mapping = aes(x = Group,
                       y = log(`Time to first reaction in seconds`))) +
  geom_boxplot(mapping = aes(fill = Group)) +
  # geom_point(size = 2, 
  #            alpha = 0.4,
  #            position = position_jitter(seed = 1, 
  #                                       width = 0.1,
  #                                       height = 0)) +
  labs(x = "",
       y = "Time to first reaction in seconds") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Time to first reaction to disruptive person") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        axis.text.x = element_text(size = 20),
        plot.title = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 18),
        )

react_group_plot

# plotting time to first reaction for all disruptions
<<<<<<< Updated upstream
# react_plot <- 
#   toi_react %>% 
#   mutate(TOI = factor(TOI,
#                         levels = c("Chatting_with_neighbour","Whispering",
#                                   "Heckling","Snipping_with_fingers",
#                                   "Drumming_with_hands","Clicking_pen",
#                                   "Head_on_table","Looking_at_phone",
#                                   "Drawing"
#                                   ),
#                         labels = c("Chatting with neighbour","Whispering",
#                                    "Heckling","Snipping with fingers",
#                                    "Drumming with hands","Clicking pen",
#                                    "Head on table","Looking at phone",
#                                    "Drawing"
#                         )
#                         )
#          ) %>% 
#   ggplot(mapping = aes(x = Group,
#                        y = `Time to first reaction in seconds`)) +
#   geom_boxplot(mapping = aes(fill = Group)) +
#   geom_point(size = 2, 
#              alpha = 0.4,
#              position = position_jitter(seed = 1, 
#                                         width = 0.1,
#                                         height = 0)) +
#   ylim(0,25) + 
#   labs(x = "") +
#   scale_fill_brewer(palette = "Set1") +
#   ggtitle("Time to first reaction to disruptive person") +
#   theme_classic() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text.x = element_text(size = 9,
#                                 angle = 90),
#     plot.title = element_text(size = 17, face = "bold"),
#     axis.title.y = element_text(size = 14),
#     ) +
#   facet_wrap(facets = vars(TOI),
#              nrow = 1,
#              strip.position = "bottom")
#     
# react_plot


# plotting time to first reaction for 3 disruptions sum up
toi_react$TOI[toi_react$TOI == "Chatting_with_neighbour" |
                toi_react$TOI == "Heckling" |
                toi_react$TOI == "Whispering"] <- "Verbal disruption"

toi_react$TOI[toi_react$TOI == "Head_on_table" |
                toi_react$TOI == "Looking_at_phone" |
                toi_react$TOI == "Drawing"] <- "Lack of eagerness"

toi_react$TOI[toi_react$TOI == "Clicking_pen" |
                toi_react$TOI == "Drumming_with_hands" |
                toi_react$TOI == "Snipping_with_fingers"] <- "Agitation"


# select relevant columns only for time to first event
toi_react <- toi_react %>% 
  select(Participant, Group, TOI,
         Time_to_first_Event.Reaction_chatting,
         Time_to_first_Event.Reaction_clicking,
         Time_to_first_Event.Reaction_drawing,
         Time_to_first_Event.Reaction_drumming,
         Time_to_first_Event.Reaction_head,
         Time_to_first_Event.Reaction_heckling,
         Time_to_first_Event.Reaction_phone,
         Time_to_first_Event.Reaction_snipping,
         Time_to_first_Event.Reaction_whispering)

# changing format with pivot_longer()
toi_react <-
  toi_react %>%
  pivot_longer(cols = contains("Time_to"),
               names_to = "Time_to_first_reaction",
               values_to = "Time_to_first_Reaction") %>%
  na.omit() %>% 
  select(Group, TOI, Participant, Time_to_first_Reaction)


# changing milliseconds into seconds
toi_react$`Time to first reaction in seconds`<- round(toi_react$Time_to_first_Reaction/1000,
                                                      digits = 2)

# plotting
react_plot_sum <-
  ggplot(data = toi_react,
         mapping = aes(x = Group,
=======
react_plot <- 
  toi_react %>% 
  mutate(TOI = factor(TOI,
                        levels = c("Chatting_with_neighbour","Whispering",
                                  "Heckling","Snipping_with_fingers",
                                  "Drumming_with_hands","Clicking_pen",
                                  "Head_on_table","Looking_at_phone",
                                  "Drawing"
                                  ),
                        labels = c("Chatting with neighbour","Whispering",
                                   "Heckling","Snipping with fingers",
                                   "Drumming with hands","Clicking pen",
                                   "Head on table","Looking at phone",
                                   "Drawing"
                        )
                        )
         ) %>% 
  ggplot(mapping = aes(x = Group,
>>>>>>> Stashed changes
                       y = `Time to first reaction in seconds`)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  # geom_point(size = 2, 
  #            alpha = 0.4,
  #            position = position_jitter(seed = 1, 
  #                                       width = 0.1,
  #                                       height = 0)) +
  # ylim(0,25) + 
  labs(x = "") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Time to first reaction to disruptive person") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=14), #change legend text font size
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
<<<<<<< Updated upstream
    strip.text.x = element_text(size = 15),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_wrap(facets = vars(TOI),
             nrow = 1,
             strip.position = "bottom")

react_plot_sum
=======
    strip.text.x = element_text(size = 8,
                                angle = 90),
    plot.title = element_text(size = 15, face = "bold")
    ) +
  facet_wrap(facets = vars(TOI),
             nrow = 1,
             strip.position = "bottom")
    
react_plot
>>>>>>> Stashed changes



############### TIME TO FIRST FIXATION ####################

# filter relevant rows only for time to first fixation
toi_fix <- toi %>% filter (TOI == "Chatting_with_neighbour"|
                               TOI == "Clicking_pen"| 
                               TOI == "Drawing"|
                               TOI == "Drumming_with_hands"| 
                               TOI == "Head_on_table"| 
                               TOI == "Heckling"|
                               TOI == "Looking_at_phone" |
                               TOI == "Snipping_with_fingers"|
                               TOI == "Whispering")

# select relevant columns only for time to first fixation
toi_fix <- toi_fix %>% 
  select(Group, Participant, TOI, Time_to_first_fixation.Disruptive_Person, Number_of_fixations.Disruptive_Person)

# remoce all NAs
toi_fix <- na.omit(toi_fix)

# changing milliseconds into seconds
toi_fix$`Time to first fixation in seconds`<- round(toi_fix$Time_to_first_fixation.Disruptive_Person/1000,
                                                      digits = 2)

toi_fix$`Number of fixations on disruptive person` <- toi_fix$Number_of_fixations.Disruptive_Person


# plotting time to first fixation for groups
fix_group_plot <- 
  ggplot(data = toi_fix,
         mapping = aes(x = Group,
                       y = log(`Time to first fixation in seconds`))) +
  geom_boxplot(mapping = aes(fill = Group)) +
  # geom_point(size = 2, 
  #            alpha = 0.4,
  #            position = position_jitter(seed = 1, 
  #                                       width = 0.1)) +
  # ylim(0,25) + 
  labs(x ="",
       y = "Time to first fixation in seconds") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Time to first fixation to disruptive person") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        axis.text.x = element_text(size = 20),
        plot.title = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 18),
  )

fix_group_plot


# plotting time to first fixation for all disruptions
# fix_plot <- 
#   toi_fix %>% 
#   mutate(TOI = factor(TOI,
#                       levels = c("Chatting_with_neighbour","Whispering",
#                                  "Heckling","Snipping_with_fingers",
#                                  "Drumming_with_hands","Clicking_pen",
#                                  "Head_on_table","Looking_at_phone",
#                                  "Drawing"
#                       ),
#                       labels = c("Chatting with neighbour","Whispering",
#                                  "Heckling","Snipping with fingers",
#                                  "Drumming with hands","Clicking pen",
#                                  "Head on table","Looking at phone",
#                                  "Drawing"
#                       )
#   )
#   ) %>% 
#   ggplot(mapping = aes(x = Group,
#                        y = `Time to first fixation in seconds`)) +
#   geom_boxplot(mapping = aes(fill = Group)) +
#   geom_point(size = 2, 
#              alpha = 0.4,
#              position = position_jitter(seed = 1, 
#                                         width = 0.1)) +
#   ylim(0,25) + 
#   labs(x ="") + 
#   scale_fill_brewer(palette = "Set1") +
#   facet_wrap(vars(TOI), 
#              nrow = 1, strip.position = "bottom") +
#   ggtitle("Time to first fixation to disruptive person") +
#   theme_classic() + 
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text.x = element_text(size = 9,
#                                 angle = 90),
#     plot.title = element_text(size = 17, face = "bold"),
#     axis.title.y = element_text(size = 14),
#   ) +
#   facet_wrap(facets = vars(TOI),
#              nrow = 1,
#              strip.position = "bottom")
# 
# fix_plot


# plotting time to first fixation for 3 disruptions sum up
toi_fix$TOI[toi_fix$TOI == "Chatting_with_neighbour" |
              toi_fix$TOI == "Heckling" |
              toi_fix$TOI == "Whispering"] <- "Verbal disruption"

toi_fix$TOI[toi_fix$TOI == "Head_on_table" |
              toi_fix$TOI == "Looking_at_phone" |
              toi_fix$TOI == "Drawing"] <- "Lack of eagerness"

toi_fix$TOI[toi_fix$TOI == "Clicking_pen" |
              toi_fix$TOI == "Drumming_with_hands" |
              toi_fix$TOI == "Snipping_with_fingers"] <- "Agitation"


# select relevant columns only for time to first fixation
toi_fix <- toi_fix %>% 
  select(Group, Participant, TOI, Time_to_first_fixation.Disruptive_Person, Number_of_fixations.Disruptive_Person)

# remoce all NAs
toi_fix <- na.omit(toi_fix)

# changing milliseconds into seconds
toi_fix$`Time to first fixation in seconds`<- round(toi_fix$Time_to_first_fixation.Disruptive_Person/1000,
                                                    digits = 2)

toi_fix$`Number of fixations on disruptive person` <- toi_fix$Number_of_fixations.Disruptive_Person

# plotting
fix_plot_sum <-
  ggplot(data = toi_fix,
         mapping = aes(x = Group,
                       y = log(`Time to first fixation in seconds`))) +
  geom_boxplot(mapping = aes(fill = Group)) +
  # geom_point(size = 2, 
  #            alpha = 0.4,
  #            position = position_jitter(seed = 1, 
  #                                       width = 0.1,
  #                                       height = 0)) +
  # ylim(0,25) + 
  labs(x = "",
       y = "Time to first fixation in seconds") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Time to first fixation to disruptive person") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=14), #change legend text font size
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
<<<<<<< Updated upstream
    strip.text.x = element_text(size = 15),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_wrap(facets = vars(TOI),
             nrow = 1,
             strip.position = "bottom")

fix_plot_sum
=======
    strip.text.x = element_text(size = 6,
                                angle = 90),
    plot.title = element_text(size = 15, face = "bold"))
>>>>>>> Stashed changes


# # plotting number of fixations for disruptive person
# number_plot <-
#   ggplot(data = toi_fix,
#          mapping = aes(x = Group,
#                        y = `Number of fixations on disruptive person`)) +
#   geom_boxplot(mapping = aes(fill = Group)) +
#   geom_point(size = 2,
#              alpha = 0.4,
#              position = position_jitter(seed = 1,
#                                         width = 0.1)) +
#   ylim(0,20) +
#   labs(x = "") +
#   scale_fill_brewer(palette = "Set1") +
#   facet_wrap(vars(TOI),
#              nrow = 1, strip.position = "bottom") +
#   ggtitle("Number of fixations on disruptive person") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text.x = element_text(size = 8,
#                                 angle = 80))
# 
# number_plot

# # arranging plots 
# grid.arrange(fix_plot, react_plot, ncol=2, nrow =1)

#################### MEAN, SD, N ############

# mean ttff
toi_fix_mean <- toi_fix %>%
  group_by(Group) %>%
  summarise("N"=n_distinct(Participant),
            "M" = round(mean(`Time to first fixation in seconds`), 2),
            "SD" = round(sd(`Time to first fixation in seconds`), 2))

# mean ttfr 
toi_react_mean <- toi_react %>%
  group_by(Group) %>%
  summarise("N"=n_distinct(Participant),
            "M" = round(mean(`Time to first reaction in seconds`), 2),
            "SD" = round(sd(`Time to first reaction in seconds`), 2))

        