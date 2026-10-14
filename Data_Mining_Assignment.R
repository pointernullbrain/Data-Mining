install.packages("arrow")
library(arrow)
library(dplyr)
library(ggplot2)
library(tidyverse)
item <- read_parquet("https://storage.data.gov.my/pricecatcher/lookup_item.parquet")
records <- read_parquet("https://storage.data.gov.my/pricecatcher/pricecatcher_2025-01.parquet")
premise <- read_parquet("https://storage.data.gov.my/pricecatcher/lookup_premise.parquet")

df_merged = records %>% left_join(item, by = "item_code") %>% left_join(premise, by = "premise_code")

sum(is.na(df_merged))
colSums(is.na(df_merged))

df_reduced <- df_merged%>%filter(date == "2025-01-01")#, state == "Selangor")
boxplot(df_reduced$price)
summary(df_reduced)

ggplot(df_reduced, aes(x = price)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Raw Price Distribution", x = "Price (MYR)", y = "Count")

# Frequency Distribution: Premises by State
ggplot(premise, aes(x = fct_infreq(state))) +
  geom_bar(fill = "pink") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Premise Count by State", x = "State", y = "Count")

#test
a <- 1+4
a
