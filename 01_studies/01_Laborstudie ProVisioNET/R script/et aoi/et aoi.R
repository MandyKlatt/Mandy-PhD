### ProVisioNET Eyetracking AOI Coding Data 

library(needs)
needs(tidyverse,
      psych,
      moments,
      sjPlot,
      DescTools,
      irr,
      readxl, 
      gridExtra,
      janitor)

# suppress "summarize" info. 
# if this line is ommitted, each table using the summarize function will be accompanied with a warning from the console
options(dplyr.summarise.inform = FALSE)


# read in expert data
expert_aoi1 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_202_203_204_interval_complete.tsv",
                       locale = locale(decimal_mark = ","))

expert_aoi2 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_205_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

expert_aoi3 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_206_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

expert_aoi4 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_207_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

# combine the expert df
expert_aoi <- rbind(expert_aoi1, 
                    expert_aoi2, 
                    expert_aoi3, 
                    expert_aoi4)
                    


# read in novice data
novice_aoi1 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_101_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))
                      
novice_aoi2 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_102_103_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi3 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_104_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi4 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_105_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi5 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_106_107_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))


novice_aoi6 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_108-111_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi7 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_112_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi8 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_113-115_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi9 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_116_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

novice_aoi10 <- read_tsv(file = "data/ProVisioNET_study_glasses_metrics_117_interval_complete.tsv",
                        locale = locale(decimal_mark = ","))

# combine the novice df
novice_aoi <- rbind(novice_aoi1, 
                    novice_aoi2, 
                    novice_aoi3, 
                    novice_aoi4, 
                    novice_aoi5, 
                    novice_aoi6, 
                    novice_aoi7,
                    novice_aoi8,
                    novice_aoi9,
                    novice_aoi10)


# combine the both df
df_aoi <- rbind(novice_aoi,
                expert_aoi)



############### TIME TO FIRST FIXATION ####################

# filter relevant rows only for time to first reaction
df_aoi <- df_aoi %>% filter (TOI == "Lesson" |
                               TOI == "Chatting_with_neighbour"|
                               TOI == "Clicking_pen"| 
                               TOI == "Drawing"|
                               TOI == "Drumming_with_hands"| 
                               TOI == "Head_on_table"| 
                               TOI == "Heckling"|
                               TOI == "Looking_at_phone" |
                               TOI == "Snipping_with_fingers"|
                               TOI == "Whispering")


# select relevant columns only for time to first fixation
df_aoi <- df_aoi %>% 
  select(Participant, 
         Group,
         TOI,
         Time_to_first_fixation.Disruptive_Person,
         starts_with("Total_duration_of_fixations"),
         starts_with("Number_of_fixations"))


# # remove all NAs
# df_aoi <- na.omit(df_aoi)

# changing milliseconds into seconds
df_aoi$Time_to_first_fixation_seconds.Disruptive_Person <- round(df_aoi$Time_to_first_fixation.Disruptive_Person/1000,
                                                    digits = 2)

# toi_fix$`Number of fixations on disruptive person` <- toi_fix$Number_of_fixations.Disruptive_Person


########################### TIME TO FIRST FIXATION ON DISRUPTIVE PERSON ######################

# plotting time to first fixation for groups
fix_group_plot <- 
  ggplot(data = df_aoi,
         mapping = aes(x = Group,
                       y = Time_to_first_fixation_seconds.Disruptive_Person)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 2, 
             alpha = 0.4,
             position = position_jitter(seed = 1, 
                                        width = 0.1)) +
  ylim(0,25) + 
  labs(x ="") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Time to first fixation in seconds to disruptive person") +
  theme_classic() + 
  theme(legend.position="none",
        axis.text.x = element_text(size = 11),
        plot.title = element_text(size = 15, face = "bold"))

fix_group_plot


# plotting time to first fixation for all disruptions
fix_plot <- 
  ggplot(data = df_aoi,
         mapping = aes(x = Group,
                       y = Time_to_first_fixation_seconds.Disruptive_Person)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 2, 
             alpha = 0.4,
             position = position_jitter(seed = 1, 
                                        width = 0.1)) +
  ylim(0,25) + 
  labs(x ="") + 
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(vars(TOI), 
             nrow = 1, strip.position = "bottom") +
  ggtitle("Time to first fixation in seconds to disruptive person") +
  theme_classic() + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 6,
                                angle = 90),
    plot.title = element_text(size = 15, face = "bold"))

fix_plot


########################### TOTAL DURATION OF FIXATIONS IN AOIS ######################

# selecting relevant columns
df_aoi_totaldur <- df_aoi %>% 
  select(Group,
         TOI,
         starts_with("Total_duration_of_fixations"))


# changing format from wide to long
df_aoi_totaldur <- df_aoi_totaldur %>% pivot_longer(
                                        cols = contains("Total_duration"),
                                        names_to = "Total_Durations_Of_Fixations",
                                        values_to = "Milliseconds")


# remove all NAs
df_aoi_totaldur <- na.omit(df_aoi_totaldur)


# changing milliseconds into seconds
df_aoi_totaldur$Seconds <- round(df_aoi_totaldur$Milliseconds/1000,
                        digits = 2)


# rename values
df_aoi_totaldur <- df_aoi_totaldur %>% 
  mutate(Total_Durations_Of_Fixations = recode(Total_Durations_Of_Fixations, 
                                               Total_duration_of_fixations.Anna = 'StudentA',
                                               Total_duration_of_fixations.Bianca = 'StudentB',
                                               Total_duration_of_fixations.Board_Screen = 'Board_Screen',
                                               `Total_duration_of_fixations.Carl(a)` = 'StudentC',
                                               Total_duration_of_fixations.Classroom_Others = 'Classroom_Others',
                                               Total_duration_of_fixations.Disruptive_Person = 'Disruptive_Person',
                                               Total_duration_of_fixations.Material_Students = 'Material_Students',
                                               Total_duration_of_fixations.Material_Teacher = 'Material_Teacher',
                                               Total_duration_of_fixations.Nametag_Anna = 'NametagA',
                                               Total_duration_of_fixations.Nametag_Bianca = 'NametagB',
                                               `Total_duration_of_fixations.Nametag_Carl(a)` = 'NametagC'))
         
# plotting total duration of fixations for groups
totaldur_group_plot <- 
  ggplot(data = df_aoi_totaldur,
         mapping = aes(x = Group,
                       y = Seconds)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 2, 
             alpha = 0.1,
             position = position_jitter(seed = 1, 
                                        width = 0.1)) +
  ylim(0,25) + 
  labs(x ="") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Total Duration of Fixations in AOIs") +
  theme_classic() + 
  theme(legend.position="none",
        axis.text.x = element_text(size = 11),
        plot.title = element_text(size = 15, face = "bold"))

totaldur_group_plot



# plotting total duration of fixations for all disruptions
totaldur_plot <- 
  df_aoi_totaldur %>% 
  # mutate(Total_durations_of_Fixations = factor(Total_durations_of_Fixations,
  #                                              levels = unique(.$Total_durations_of_Fixations),
  #                                              labels = c("StudentA", 
  #                                                         "StudentB",
  #                                                         "StudentC",
  #                                                         "Disruptive_Person",
  #                                                         "Nametag_B",
  #                                                         "Board_Screen",
  #                                                         "Classroom_Others",
  #                                                         "Matieral_Teacher",
  #                                                         "Nametag_A",
  #                                                         "Nametag_C",
  #                                                         "Material_Students"))) %>% 
  ggplot(data = df_aoi_totaldur,
         mapping = aes(x = Group,
                       y = Seconds)) +
  # geom_violin(mapping = aes(fill = Group)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 1,
             alpha = 0.1,
             position = position_jitter(seed = 1,
                                        width = 0.1)) +
  ylim(0,25) + 
  labs(x ="") + 
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(vars(Total_Durations_Of_Fixations), 
             nrow = 1, strip.position = "bottom") +
  ggtitle("Total Duration of Fixations in AOIs") +
  theme_classic() + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 6,
                                angle = 90),
    plot.title = element_text(size = 15, face = "bold"))

totaldur_plot


########################### NUMBER OF FIXATIONS IN AOIS ######################
# selecting relevant columns
df_aoi_number <- df_aoi %>% 
  select(Group,
         TOI,
         starts_with("Number_of_fixations"))


# changing format from wide to long
df_aoi_number <- df_aoi_number %>% pivot_longer(
  cols = contains("Number_of_fixations"),
  names_to = "Number_Of_Fixations_in_AOIs",
  values_to = "Number")


# remove all NAs
df_aoi_number <- na.omit(df_aoi_number)

# rename values
df_aoi_number <- df_aoi_number %>% 
  mutate(Number_Of_Fixations_in_AOIs = recode(Number_Of_Fixations_in_AOIs,
                                          Number_of_fixations.Anna = 'StudentA',
                                          Number_of_fixations.Bianca = 'StudentB',
                                          Number_of_fixations.Board_Screen = 'Board_Screen',
                                          `Number_of_fixations.Carl(a)` = 'StudentC',
                                          Number_of_fixations.Classroom_Others = 'Classroom_Others',
                                          Number_of_fixations.Disruptive_Person = 'Disruptive_Person',
                                          Number_of_fixations.Material_Students = 'Material_Students',
                                          Number_of_fixations.Material_Teacher = 'Material_Teacher',
                                          Number_of_fixations.Nametag_Anna = 'NametagA',
                                          Number_of_fixations.Nametag_Bianca = 'NametagB',
                                          `Number_of_fixations.Nametag_Carl(a)` = 'NametagC'))


# plotting number of fixations in AOIs for groups
number_group_plot <- 
  ggplot(data = df_aoi_number,
         mapping = aes(x = Group,
                       y = Number)) +
  # geom_violin(mapping = aes(fill = Group)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 2,
             alpha = 0.1,
             position = position_jitter(seed = 1,
                                        width = 0.1)) +
  ylim(0,25) + 
  labs(x ="") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Number of Fixations in AOIs") +
  theme_classic() + 
  theme(legend.position="none",
        axis.text.x = element_text(size = 11),
        plot.title = element_text(size = 15, face = "bold"))

number_group_plot



# plotting number of fixations in all AOIs
number_plot <- 
  ggplot(data = df_aoi_number,
       mapping = aes(x = Group,
                     y = Number)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  # geom_violin(mapping = aes(fill = Group)) +
  ylim(0,25) + 
  labs(x ="") + 
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(vars(Number_Of_Fixations_in_AOIs), 
             nrow = 1, strip.position = "bottom") +
  ggtitle("Number of Fixations in AOIs") +
  theme_classic() + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 6,
                                angle = 90),
    plot.title = element_text(size = 15, face = "bold"))

number_plot

