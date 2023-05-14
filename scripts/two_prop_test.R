# This script will show
# My process of running a 2 proportion test

# loading in packages
library(tidyverse)
library(forcats)
library(kableExtra)
library(scales)

# let p_wet = proportion of success for wet method
# p_dry = proportion of success for dry method

# creating analysis data set from raw data
pong_test <- pong

# create a column of trial IDs in the test data
pong_test$trial <- c(1:100)

# cleaning / re-coding data
pong_test <- pong_test |> 
  select(trial, method, success, which_cup) |> 
  mutate(method = as.factor(method),
       which_cup = as.factor(which_cup),
       which_cup = fct_recode(.f = which_cup,
                               None = "0"))

mylabel <- label_percent(scale = 100, suffix = "%")

# creating a summary table
pong_summary_table <- pong_test |> 
  mutate(
    method = fct_recode(.f = method,
                        "Dry" = "D",
                        "Wet" = "W")) |> 
  group_by(method) |> 
  summarise(total.attempts = n(),
            frequency = sum(success),
            pct = mean(success)) |> 
  mutate(
    pct = mylabel(pct)
  )



# outputting a pretty version of pong_summary_table
pong_summary_table |>
  kbl(caption = "<center>Table 1:<br>
                <center>Frequencies and Percentages of 
                  Successful Shots by Cup Pong Method",
      format = "html",
      col.names = 
        c("Method", "Total Attempts", "Count of Success", "Percentage Made (%)"),
      align = "cccr") |> 
  kable_classic(full_width = F,  html_font = "Source Sans Pro") |> 
  kable_styling(bootstrap_options = "striped")



# ANALYSIS

# finding number of successes and failures for each method
prop.table <- pong_test |> 
  group_by(method) |> 
  summarise(successes = sum(success)) #summing a binary variable gives count of successes


# creating objects to use in the prop.test

# 50 trials were run for each method
num_shots <- c(50,50)

success_dry <- 19
success_wet <- 19

# vector of both proportions 
two_counts <- c(success_dry, success_wet)



# running a two-sided 2 proportion test
prop.test(x = two_counts, n = num_shots, 
          alternative='two.sided', conf.level = 0.95)
