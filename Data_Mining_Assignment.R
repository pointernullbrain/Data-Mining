# Load library
install.packages("arrow")

library(arrow)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Read files
item <- read_parquet("https://storage.data.gov.my/pricecatcher/lookup_item.parquet")
records <- read_parquet("https://storage.data.gov.my/pricecatcher/pricecatcher_2025-01.parquet")
premise <- read_parquet("https://storage.data.gov.my/pricecatcher/lookup_premise.parquet")

df_merged = records %>% left_join(item, by = "item_code") %>% left_join(premise, by = "premise_code")

sum(is.na(df_merged))
colSums(is.na(df_merged))

df_reduced <- df_merged%>%filter(date == "2025-01-01")#, state == "Selangor")
boxplot(df_reduced$price)
summary(df_reduced)
