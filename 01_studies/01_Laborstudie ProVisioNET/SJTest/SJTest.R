# load packages
library(needs)
needs(tidyverse,
      lubridate,
      viridis,
      grid,
      gridExtra,
      cowplot,
      readxl,
      ARTofR)

# read in data
df_sjt <- read_csv2(file = "C:/Users/mk99feta/OneDrive/Dokumente/GitHub/Mandy-PhD/01_studies/01_Laborstudie ProVisioNET/SJTest/data/SJT.csv")

# select relevant columns --> only mean
df_sjt <- df_sjt %>% select(UI06_05, SJT_AL_gek, SJT_ST_gek, SJT_R_gek, SJT_KF_gek)

# rename columns
df_sjt <- rename(df_sjt, c("Group" = "UI06_05",
                           "Monitoring" = "SJT_AL_gek",
                           "Managing momentum" = "SJT_ST_gek",
                           "Rules and routines" = "SJT_R_gek",
                           "All" = "SJT_KF_gek"))

# remove NAs
df_sjt <- na.omit(df_sjt)

# changing from wide to long format
df_sjt_long <- df_sjt %>% 
  pivot_longer(!Group, names_to = "Facets Classroom Management", values_to = "Mean")

# define expert and novice with ifelse function
df_sjt_long$Group = ifelse(df_sjt_long$Group < 200, "Novice","Expert")

# plotting mean of all aspects 
mean_plot <- 
  ggplot(data = df_sjt_long,
         mapping = aes(x = Group,
                       y = Mean)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 2, 
             alpha = 0.4,
             position = position_jitter(seed = 1, 
                                        width = 0.1)) +
  labs(x = "") +
  ylim(0,1) + 
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(vars(`Facets Classroom Management`), 
             nrow = 1, strip.position = "bottom") +
  theme_classic()+
  ggtitle("Situational Judgment Test of Strategic Knowledge of Classroom Management") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

mean_plot

#################### MEAN, SD, N ############

# mean SJT ALL
sjt_mean <- df_sjt_long %>%
  filter(`Facets Classroom Management` == "All") %>%
  group_by(Group) %>%
  summarise("M" = round(mean(Mean), 2),
            "SD" = round(sd(Mean), 2))


# mean SJT Managing Momentum
sjt_mm <- df_sjt_long %>%
  filter(`Facets Classroom Management` == "Managing momentum") %>%
  group_by(Group) %>%
  summarise("M" = round(mean(Mean), 2),
            "SD" = round(sd(Mean), 2))

# mean SJT Monitoring
sjt_m <- df_sjt_long %>%
  filter(`Facets Classroom Management` == "Monitoring") %>%
  group_by(Group) %>%
  summarise("M" = round(mean(Mean), 2),
            "SD" = round(sd(Mean), 2))


# mean SJT Rules and routins
sjt_r <- df_sjt_long %>%
  filter(`Facets Classroom Management` == "Rules and routines") %>%
  group_by(Group) %>%
  summarise("M" = round(mean(Mean), 2),
            "SD" = round(sd(Mean), 2))

