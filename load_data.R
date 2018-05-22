library(tidyverse)
library(janitor)

train_df <- read_csv("data/cs-training.csv")
test_df <- read_csv("data/cs-test.csv")

train_df <- train_df %>% clean_names()
test_df <- test_df %>% clean_names()
