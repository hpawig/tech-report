#loading in necessary packages
library(here)
library(readxl)

#read in data
pong <- read_excel(here::here('data',
                              'data_raw',
                              'raw pong data.xlsx'))
