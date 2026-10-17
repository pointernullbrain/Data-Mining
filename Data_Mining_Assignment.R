# STQD6414 Data Mining - Assignment 1

#-------------------------------------------
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

# Initial structures
str(item)
str(records)
str(premise)

# Initial descriptive statistics
summary(item)
summary(records)
summary(premise)

#-------------------------------------------
# Data merging
df_merged <- records %>%
  left_join(item, by = "item_code") %>%
  left_join(premise, by = "premise_code")

# Data filter (date / & state)
df_reduced <- df_merged%>%filter(date == "2025-01-01")#, 
                                 #state == "Selangor")

#-------------------------------------------
# Data cleaning
# Remove duplicates (if any)
sum(duplicated(df_reduced))
df_reduced <- df_reduced %>% distinct()

# Check missing values in raw records
sum(is.na(df_reduced))
colSums(is.na(df_reduced))

df_clean <- na.omit(df_reduced)

# Check again duplicates and missing values
sum(duplicated(df_clean))
sum(is.na(df_clean))
colSums(is.na(df_clean))

#-------------------------------------------
boxplot(df_clean$price)
summary(df_clean)
