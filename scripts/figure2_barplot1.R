library(tidyverse)
library(ggtext)


# make sure to read in data from the file, reading_raw_data.R 
# then run the R script two_prop_test.R 

#### Figure 3 ####
# get proportions per method

pong_overall_counts <- pong_test |>
  mutate(
    method = fct_recode(.f = method,
                        "Dry" = "D",
                        "Wet" = "W"),
    
    success_by_method = case_when(
      success == 1 & method == "Dry" ~ "Success, Dry",
      success == 1 & method == "Wet" ~ "Success, Wet",
      TRUE ~ "Failure"),
    
    success_by_method = factor(success_by_method, levels = c("Failure",
                                                             "Success, Wet",
                                                             "Success, Dry")),
    overall = "Overall")


# making the plot
pong_overall_counts |> 
  ggplot(mapping = aes(x = overall,
                       fill = success_by_method)) + 
  geom_bar(stat = "count",
           position = "stack") +
  labs(title = "Figure 2: Overall Successes of the 100 Shot Attempts",
       subtitle = "Successes with the 
       <span style='color:#005AB5'>Dunk <span style='color:#005AB5'>Method 
       <span style='color:black'>and
        <span style='color:#DC3220'>Dry <span style='color:#DC3220'>Ball Method",
       caption = "Visual depiction of total successful vs. unsuccessful shots. 
       The successful shot segment is split by the method type (Dunk or Dry ball).
       38 of 100 total shot attempts successfully landed into a cup.") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.subtitle = element_markdown(),
        legend.position = "none",
        plot.caption = element_text(hjust = 1,
                                    size = 7,
                                    vjust = 0)) +
  scale_fill_manual(values = c('gray48','#005AB5', '#DC3220')) +
  annotate("text", x = .75, y = 36, 
           label = "Total Successes (38)",
           color = "white", size = 3) +
  annotate("text", x = 1.35, y = 98, color = 'lightgrey', size = 3, label = "Failures") +
  scale_y_continuous(breaks = c(0,25,38,50,75,100))


# saving plot as png
ggsave(filename = "barplot_overall_success.png",
       path = "/Users/hannahpawig/Desktop/STAT 365 Report/tech_report/results")
