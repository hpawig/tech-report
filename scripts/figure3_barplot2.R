library(tidyverse)
library(ggtext)


# make sure to read in data from the file, reading_raw_data.R 
# then run the R script two_prop_test.R 

#### Figure 2 ####
# getting total proportion of success and failure
pong_total_counts <- pong_test |> 
  ungroup() |> 
  select(method, success) |> 
  mutate(method = "Combined",
         shot_made = case_when(
           success == 1 ~ "Yes",
           success == 0 ~ "No"))

pong_counts <- pong_test |> 
  mutate(
    method = fct_recode(.f = method,
                        "Dry" = "D",
                        "Wet" = "W"),
    shot_made = case_when(
      success == 1 ~ "Yes",
      success == 0 ~ "No")) |> 
  group_by(shot_made, method)

# create the plot

pong_counts |>
  mutate(
    shot_made = fct_recode(.f = shot_made,
                           "Missed Shot" = "No",
                           "Attempt Successful" = "Yes")) |> 
  ggplot(mapping = aes(x = method,
                       fill = shot_made)) +
  geom_bar(stat = "count") +
  labs(title = "Figure 3: Breakdown of Shot Attempts",
       subtitle = "For Each Playing Method",
       x = "Playing Method") +
  scale_fill_manual(guide_legend(title = "Attempt Status"),
                    values = c('firebrick','salmon')) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8))

ggsave("barplot_by_method.png", path = "/Users/hannahpawig/Desktop/STAT 365 Report/tech_report/results")




