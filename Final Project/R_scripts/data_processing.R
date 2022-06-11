# DATA Processing 
library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(xts)
library(forecast)
library(zoo)
library(corrplot)
library(kernlab)
library(prophet)
library(plotly)
library(rules)
options(tigris_use_cache = TRUE)

#--------------------------------------------------
# ** initial cleaning 
# import unprocessed data
unprocessed = read.csv("Final Project/data/unprocessed_data.csv")
head(unprocessed)

# dealing NAs and character values(all variables should be numeric)
unprocessed = unprocessed[-1,]
date = unprocessed[,1]

# delete NA in the first row 
unprocessed = unprocessed[,-1] %>% mutate_if(is.character, as.numeric)

unprocessed$DATE = date
unprocessed = unprocessed %>%
  select(DATE, everything())

# delete variables that have at least 8 missing values 
processed_data = unprocessed %>% select(-avg_price_pipedgas_la, -new_patent_ass_la,
                                        -cpi_allitems_la, -us_interest_rate,
                                        -economics_cond_index_la) %>% 
  head(-2)

# Checker: no missing value 
sapply(processed_data, function(x) sum(is.na(x)))

# store the processed data to the data file 
write.csv(processed_data, "Final Project/data/processed_data.csv", row.names = FALSE)


#-----------------------------------------------------------
# import processed data and process it for model building 
model_data = read.csv("Final Project/data/processed_data.csv")

data = model_data %>%
  mutate(DATE, DATE = as.Date.character(DATE))

# I decided to only use local economic indicators to simplify the model
data = data %>% select(DATE, contains("la"))
# I don't want to train my model in pandemic period

# train and test period 
data_2016 = data[1:320,] 
# test for further forecast unemployment rate in 2017
data_2017 = data[321:332,]

# plot my selected time range 1990-2015
data_2016 %>% plot_time_series(DATE, unemploy_rate_la)

#-----------------------------------------------------------



#-----------------------------------------------------------
# Creating lag and time features for machine learning algorithm 
data_2016_full <- data_2016 %>%
  bind_rows(
    # add future window for lag variables 
    future_frame(.data = ., .date_var = DATE, .length_out = 1)
  ) %>% 
  # add auto correlated lags 1, 4, 12 months (month, quarter, year)
  # used to predict seasonality 
  tk_augment_lags(unemploy_rate_la, .lags = 1) %>%
  tk_augment_lags(unemploy_rate_la, .lags = 4) %>%
  tk_augment_lags(unemploy_rate_la, .lags = 12) %>%
  tk_augment_lags(contains("la"), .lags = 1) %>%
  tk_augment_lags(contains("la"), .lags = 4) %>%
  tk_augment_lags(contains("la"), .lags = 12)

# view the new data 
data_2016_full

# split the data
new_splits <- initial_time_split(data_2016_full, prop = 0.9)
new_train_2016 <- training(new_splits)
new_test_2016 <- testing(new_splits)

# create recipes

# base recipe 
recipe_spec_base <- recipe(unemploy_rate_la ~., data = new_train_2016) %>%
  step_timeseries_signature(DATE) %>%
  # feature removal
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)|(day)|(week)")) %>%
  # standardization
  step_normalize(matches("(index.num)|(year)|(issue)")) %>%
  # month feature is converted to dummy variables 
  step_dummy(all_nominal(), one_hot = TRUE) 

# I did not add interaction and fourier series features since I cannot decide 
# to apply these feauture to which variables 

# spline recipe 
recipe_spec_1 <- recipe_spec_base %>% 
  step_rm(DATE) %>%
  step_ns(ends_with("index.num")) %>%
  step_rm(matches("lag"))

# lag recipe 
recipe_spec_2 <- recipe_spec_base %>%
  step_rm(DATE) %>%
  step_naomit(matches("lag"))





