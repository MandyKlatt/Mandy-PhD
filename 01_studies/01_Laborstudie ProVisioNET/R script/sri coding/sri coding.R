### ProVisioNET SRI Coding Data 

library(needs)
needs(tidyverse,
      psych,
      moments,
      sjPlot,
      DescTools,
      irr,
      readxl, 
      gridExtra,
      cowplot)

# suppress "summarize" info. 
# if this line is omitted, each table using the summarize function will be accompanied with a warning from the console
# options(dplyr.summarise.inform = FALSE)

# read in data and combine multiple excel sheets into a single table 
sri <- 
  excel_sheets("data/Coding_SRI.xlsx") %>% 
  map_df(~read_xlsx("data/Coding_SRI.xlsx",.)) %>% 
  select(Group, Event, `Disruption Factor`, `Confident Factor`) %>% # select relevant columns 
  filter(Group %in% c(101:125, 202:217))# filter relevant rows

 
# # counting drop outs (-99 not perceived, -100 not answered)
# # all dropout
# sri$dropout <- 
#   length(which(sri$`Disruption Factor`== -99 |
#                sri$`Disruption Factor`== -100 |
#                 sri$`Confident Factor` == -100)) %>% 
#   as.numeric(sri$dropout)
# 
# # not perceived
# sri$not_perceived <-
#   length(which(sri$`Disruption Factor`== -99)) %>% 
#   as.numeric(sri$not_perceived)
# 
# # count all events and calculate percentage of dropouts
# sri$sum_events <-
#   length(summary(as.factor(sri$Group))) %>% 
#   as.numeric(sri$sum_events) %>% 
#   mutate(all_dropout_percent= (dropout/sum_events)*100)


# drop out all drop out
sri <- 
  sri %>% 
  filter(!`Disruption Factor` == -100,
         !`Confident Factor` == -100,
         !`Disruption Factor`== -99,
         !`Confident Factor` == -99,
         !`Disruption Factor`== -88,
         !`Confident Factor` == -88)
  
         
# define expert and novice with ifelse function
sri$Group = ifelse(sri$Group < 200, "Novice","Expert")

# create a new data frame with both factors
sri_disrup <- subset.data.frame(sri, select = c(Group, Event, `Disruption Factor`))
sri_confi <- subset.data.frame(sri, select = c(Group, Event, `Confident Factor`))


# plotting Disruption factor for groups
dist_group_plot <- 
  sri_disrup %>% 
  mutate(Group = factor(Group,
                        levels = c("Novice", 
                                   "Expert"
                                   )
                        )
         ) %>% 
  ggplot(mapping = aes(x = Group,
                       y = `Disruption Factor`)) +
  geom_boxplot(mapping = aes(fill = Group),
               outlier.shape = NA) +
  geom_point(size = 1,
             alpha = 0.4,
             position = position_jitter(seed = 1,
                                        width = 0.1,
                                        height = 0.1)) +
  scale_x_discrete(limits = c("Novice", "Expert")) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
  labs(x = "",
       y = "Disruption Factor") + 
  scale_fill_brewer(palette  = "RdBu") +  
  ggtitle("How disruptive was the event for you?") +
  theme_cowplot() + 
  theme(
    legend.position="none",
<<<<<<< Updated upstream
    axis.text.x = element_text(size = 23),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    plot.title = element_text(size = 25, 
                              face = "bold"),
    )
=======
    axis.text.x = element_text(size = 15),
    plot.title = element_text(size = 19, face = "bold"),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15))
>>>>>>> Stashed changes

dist_group_plot

ggsave(plot = dist_group_plot,
       filename = "plots/dist_group_plot.svg",
       height = 8,
       width = 8,
       units = "in")


<<<<<<< Updated upstream
# # plotting Disruption factor for 3 disruptions sum up
# sri_disrup$Event[sri_disrup$Event == "chatting" |
#                    sri_disrup$Event == "heckling" |
#                    sri_disrup$Event == "whispering"] <- "Verbal disruption"
# 
# sri_disrup$Event[sri_disrup$Event == "head on table" |
#                    sri_disrup$Event == "looking at phone" |
#                    sri_disrup$Event == "drawing"] <- "Lack of eagerness"
# 
# sri_disrup$Event[sri_disrup$Event == "clicking pen" |
#                    sri_disrup$Event == "drumming" |
#                    sri_disrup$Event == "snipping"] <- "Agitation"
# 
# 
# 
# 
# # plotting
# sri_disrup_sum <-
#   ggplot(data = sri_disrup,
#          mapping = aes(x = Group,
#                        y = `Disruption Factor`)) +
#   geom_boxplot(mapping = aes(fill = Group)) +
#   geom_point(size = 1, 
#              alpha = 0.4,
#              position = position_jitter(seed = 1, 
#                                         width = 0.1,
#                                         height = 0.1)) +
#   scale_x_discrete(limits = c("Novice", "Expert")) +
#   ylim(0,10) + 
#   labs(x = "") +
#   ylab("Disruption Factor") +
#   scale_fill_brewer(palette  = "RdBu") +  
#   ggtitle("How disruptive was the event for you?") +
#   theme_classic() +
#   theme(
#     legend.title = element_text(size=14), #change legend title font size
#     legend.text = element_text(size=14), #change legend text font size
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text.x = element_text(size = 15),
#     plot.title = element_text(size = 20, face = "bold"),
#     axis.title.y = element_text(size = 10),
#   ) +
#   facet_wrap(facets = vars(Event),
#              nrow = 1,
#              strip.position = "bottom")
# 
# sri_disrup_sum
# 
=======
# plotting Disruption factor for all disruptions
dist_plot <- 
  sri_disrup %>% 
  mutate(Event = factor(Event,
                      levels = c("chatting",
                                 "whispering",
                                 "heckling",
                                 "snipping",
                                 "drumming",
                                 "clicking",
                                 "looking at phone",
                                 "head on table",
                                 "drawing"
                      ),
                      labels = c("Chatting",
                                 "Whispering",
                                 "Heckling",
                                 "Snipping",
                                 "Drumming",
                                 "Clicking pen",
                                 "Looking at phone",
                                 "Head on table",
                                 "Drawing"
                      )
  )) %>%
  ggplot(data = sri_disrup,
         mapping = aes(x = Group,
                       y = Disruption_Factor)) +
  geom_boxplot(mapping = aes(fill = Group)) +
  geom_point(size = 2, 
             alpha = 0.4,
             position = position_jitter(seed = 1, 
                                        width = 0.1,
                                        height = 0)) +
  labs(x = "",
       y = "Disruption Factor") + 
  ylim(0,10)+
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(vars(Event), 
             nrow = 1, strip.position = "bottom") +
  ggtitle("How disruptive was the event for you?") +
  theme_classic() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 8, 
                                    angle = 90),
        plot.title = element_text(size = 15, face = "bold"))
>>>>>>> Stashed changes


# # plotting Disruption factor for all disruptions
# dist_plot <- 
#   sri_disrup %>% 
#   mutate(Event = factor(Event,
#                       levels = c("chatting",
#                                  "whispering",
#                                  "heckling",
#                                  "snipping",
#                                  "drumming",
#                                  "clicking pen",
#                                  "looking at phone",
#                                  "head on table",
#                                  "drawing"
#                       ),
#                       labels = c("Chatting",
#                                  "Whispering",
#                                  "Heckling",
#                                  "Snipping",
#                                  "Drumming",
#                                  "Clicking pen",
#                                  "Looking at phone",
#                                  "Head on table",
#                                  "Drawing"
#                                  )
#   )) %>%
#   ggplot(mapping = aes(x = Group,
#                        y = Disruption_Factor)) +
#   geom_boxplot(mapping = aes(fill = Group)) +
#   geom_point(size = 2, 
#              alpha = 0.4,
#              position = position_jitter(seed = 1, 
#                                         width = 0.1,
#                                         height = 0)) +
#   labs(x = "",
#        y = "Disruption Factor") + 
#   ylim(0,10)+
#   scale_fill_brewer(palette = "Set1") +
#   facet_wrap(vars(Event), 
#              nrow = 1, strip.position = "bottom") +
#   ggtitle("How disruptive was the event for you?") +
#   theme_classic() + 
#   theme(axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         strip.text.x = element_text(size = 8, 
#                                     angle = 90),
#         plot.title = element_text(size = 15, face = "bold"))
# 
# 
# dist_plot


# plotting Confident factor for group
confi_group_plot <- 
  sri_confi %>% 
  mutate(Group = factor(Group, 
                        levels = c("Novice",
                                   "Expert")
                        )
         ) %>% 
  ggplot(mapping = aes(x = Group,
                       y = `Confident Factor`)) +
  geom_boxplot(mapping = aes(fill = Group), 
               outlier.shape = NA) +
  labs(x = "", 
       y = "Confident Factor") +
  scale_x_discrete(limits = c("Novice", "Expert")) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
  geom_point(size = 1,
             alpha = 0.4,
             position = position_jitter(seed = 1,
                                        width = 0.1,
                                        height = 0.1)) +
  scale_fill_brewer(palette  = "RdBu") +  
  ggtitle("How confident did you feel \ndealing with this event?") +
  theme_cowplot() + 
  theme(
    legend.position="none",
    axis.text.x = element_text(size = 23),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    plot.title = element_text(size = 25, 
                              face = "bold"),
    )

confi_group_plot

ggsave(plot = confi_group_plot,
       filename = "plots/confi_group_plot.svg",
       height = 8,
       width = 8,
       units = "in")


# # plotting Confident factor for 3 disruptions sum up
# sri_confi$Event[sri_confi$Event == "chatting" |
#                   sri_confi$Event == "heckling" |
#                   sri_confi$Event == "whispering"] <- "Verbal disruption"
# 
# sri_confi$Event[sri_confi$Event == "head on table" |
#                    sri_confi$Event == "looking at phone" |
#                    sri_confi$Event == "drawing"] <- "Lack of eagerness"
# 
# sri_confi$Event[sri_confi$Event == "clicking pen" |
#                   sri_confi$Event == "drumming" |
#                   sri_confi$Event == "snipping"] <- "Agitation"
# 
# 
# 
# 
# # plotting
# sri_confi_sum <-
#   ggplot(data = sri_confi,
#          mapping = aes(x = Group,
#                        y = `Confident Factor`)) +
#   geom_boxplot(mapping = aes(fill = Group), 
#                outliner.shape = NA) +
#   geom_point(size = 1, 
#              alpha = 0.4,
#              position = position_jitter(seed = 1, 
#                                         width = 0.1,
#                                         height = 0.1)) +
#   scale_x_discrete(limits = c("Novice", "Expert")) +
#   ylim(0,10) + 
#   labs(x = "") +
#   ylab("Confident Factor") +
#   scale_fill_brewer(palette  = "RdBu") +  
#   ggtitle("How confident did you feel dealing with this event?") +
#   theme_classic() +
#   theme(
#     legend.title = element_text(size=14), #change legend title font size
#     legend.text = element_text(size=14), #change legend text font size
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text.x = element_text(size = 10),
#     plot.title = element_text(size = 20, face = "bold"),
#     axis.title.y = element_text(size = 16),
#   ) +
#   facet_wrap(facets = vars(Event),
#              nrow = 1,
#              strip.position = "bottom")
# 
# sri_confi_sum

# # plotting confident factor for all disruptions
# confi_plot <- 
#   sri_confi %>% 
#   mutate(Event = factor(Event,
#                         levels = c("chatting",
#                                    "whispering",
#                                    "heckling",
#                                    "snipping",
#                                    "drumming",
#                                    "clicking pen",
#                                    "locking at phone",
#                                    "head on table",
#                                    "drawing"
#                         ),
#                         labels = c("Chatting",
#                                    "Whispering",
#                                    "Heckling",
#                                    "Snipping",
#                                    "Drumming",
#                                    "Clicking pen",
#                                    "Looking at phone",
#                                    "Head on table",
#                                    "Drawing"
#                         )
#   )) %>%
#   ggplot(mapping = aes(x = Group,
#                        y = Confident_Factor)) +
#   geom_boxplot(mapping = aes(fill = Group)) +
#   geom_point(size = 2, 
#              alpha = 0.4,
#              position = position_jitter(seed = 1, 
#                                         width = 0.1,
#                                         height = 0)) +
#   labs(x = "",
#        y = "Confident Factor") + 
#   ylim(0,10)+
#   scale_fill_brewer(palette = "Set1") +
#   facet_wrap(vars(Event), 
#              nrow = 1, strip.position = "bottom") +
#   ggtitle("How confident did you feel dealing with this event?") +
#   theme_classic() + 
#   theme(axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         strip.text.x = element_text(size = 8, 
#                                     angle = 90),
#         plot.title = element_text(size = 15, face = "bold"))
# 
# confi_plot
# 
# # # arranging plots 
# # grid.arrange(dist_plot, confi_plot, ncol=2, nrow =1)
# 


#################### MEAN, SD ############

# mean disrup_factor
sri_disrup_mean <- sri_disrup %>%
  group_by(Group) %>%
  summarise("M" = round(mean(`Disruption Factor`), 2),
            "SD" = round(sd(`Disruption Factor`), 2))

# mean confi_factor
sri_confi_mean <- sri_confi %>%
  group_by(Group) %>%
  summarise("M" = round(mean(`Confident Factor`), 2),
            "SD" = round(sd(`Confident Factor`), 2))

#################### T-TEST & EFFECT SIZE ############

# Disruption Factor 
# t-test for expertise differences
t.test(x = sri$`Disruption Factor`[sri$Group == "Expert"],
       y = sri$`Disruption Factor`[sri$Group == "Novice"])


# Disruption Factor 
# effect size for expertise differences
d_sri_disrup <- CohenD(x = sri$`Disruption Factor`[sri$Group == "Expert"],
                       y = sri$`Disruption Factor`[sri$Group == "Novice"],
                       na.rm = TRUE)


# Confident Factor 
# t-test for expertise differences
t.test(x = sri$`Confident Factor`[sri$Group == "Expert"],
       y = sri$`Confident Factor`[sri$Group == "Novice"])


# Confident Factor 
# effect size for expertise differences
d_sri_confi <- CohenD(x = sri$`Confident Factor`[sri$Group == "Expert"],
                     y = sri$`Confident Factor`[sri$Group == "Novice"],
                     na.rm = TRUE)


# mean & SD disruption factor
# novices
mean_disrup_nov <- 
  sri_disrup %>%
  filter(Group == "Novice") %>% 
  pull(`Disruption Factor`) %>% 
  mean() %>% 
  round(., digits = 0)

sd_disrup_nov <- 
  sri_disrup %>%
  filter(Group == "Novice") %>% 
  pull(`Disruption Factor`) %>% 
  sd() %>% 
  round(., digits = 0)

# experts
mean_disrup_exp <-
  sri_disrup %>%
  filter(Group == "Expert") %>% 
  pull(`Disruption Factor`) %>% 
  mean() %>% 
  round(., digits = 0)

sd_disrup_exp <- 
  sri_disrup %>%
  filter(Group == "Expert") %>% 
  pull(`Disruption Factor`) %>% 
  sd() %>% 
  round(., digits = 0)



# mean & SD confident factor
# novices
mean_confi_nov <- 
  sri_confi %>%
  filter(Group == "Novice") %>% 
  pull(`Confident Factor`) %>% 
  mean() %>% 
  round(., digits = 0)

sd_confi_nov <- 
  sri_confi %>%
  filter(Group == "Novice") %>% 
  pull(`Confident Factor`) %>% 
  sd() %>% 
  round(., digits = 0)

# experts
mean_confi_exp <-
  sri_confi %>%
  filter(Group == "Expert") %>% 
  pull(`Confident Factor`) %>% 
  mean() %>% 
  round(., digits = 0)

sd_confi_exp <- 
  sri_confi %>%
  filter(Group == "Expert") %>% 
  pull(`Confident Factor`) %>% 
  sd() %>% 
  round(., digits = 0)

