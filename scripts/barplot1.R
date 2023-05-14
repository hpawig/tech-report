library(tidyverse)

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

# combine both data sets
# pong_counts_cmb <- pong_total_counts |> 
# full_join(pong_counts) |> 
# mutate(
#   method = fct_relevel(levels = c("Dry", "Wet", "Combined")))
# 

# works
pong_counts |>
  ggplot(mapping = aes(x = shot_made,
                       fill = method)) +
  geom_bar(stat = "count")


#pong_counts_cmb |> 
#  ggplot(mapping = aes(x = method,
#                       fill = shot_made)) + 
#  geom_bar(stat = "count")





#### Figure 3 ####
# get proportions per method

pong_overall_counts <- pong_test |>
  mutate(
    method = fct_recode(.f = method,
                        "Dry" = "D",
                        "Wet" = "W"),
    
    success_by_method = case_when(
                          success == 1 & method == "Dry" ~ "Dry Ball Success",
                          success == 1 & method == "Wet" ~ "Wet Ball Success",
                          TRUE ~ "Failure"),
    
    success_by_method = factor(success_by_method, levels = c("Failure",
                                                             "Wet Ball Success",
                                                             "Dry Ball Success")),
    overall = "Overall"
    ) |> 
  arrange(success_by_method)


# making the plot
pong_overall_counts |> 
  ggplot(mapping = aes(x = overall,
                       fill = success_by_method)) + 
  geom_bar(stat = "count",
           position = "stack") +
  labs(title = "Breakdown of All Shot Attempts",
       subtitle = "By Play Method") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank())
  
  


