# This script will show
# My process of running a 2 proportion test

# loading in packages
library(tidyverse)
library(forcats)

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




# finding number of successes and failures for each method
prop.table <- pong_test |> 
  group_by(method) |> 
  summarise(accuracy = mean(success)) #taking the mean of a dummy variable gives a prop


# creating objects to use in the prop.test

# 50 trials were run for each method
num_shots <- c(50,50)

# storing the proportions
p_dry <- 0.38
p_wet <- 0.38

# vector of both proportions 
two_props <- c(p_dry, p_wet)



# running a two-sided 2 proportion test
prop.test(x = two_props, n = num_shots, 
          alternative='two.sided', conf.level = 0.95)

