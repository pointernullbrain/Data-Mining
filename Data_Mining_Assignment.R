# STQD6414 Data Mining - Assignment 1

#-------------------------------------------
# Load library
install.packages("arrow")

library(arrow)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

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
df_reduced <- df_merged%>%filter(date == "2025-01-01", 
                                 state == "Selangor")
# Date format (dd/mm/yy)
df_reduced$date <- format(df_reduced$date, "%d/%m/%Y")
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
# Outliers treatment (BEFORE)
par(mfrow = c(1,2))
hist(df_clean$price, main = "Price Distribution (BEFORE)")
boxplot(df_clean$price, main = "Price Boxplot (BEFORE)")
summary(df_clean)

# IQR grouped by item_code
df_clean_iqr <- df_clean %>%
  group_by(item_code) %>%
  mutate(
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - (1.5 * IQR), 
    upper_bound = Q3 + (1.5 * IQR)
  ) %>%
  filter(price >= lower_bound & price <= upper_bound) %>%
  ungroup()

hist(df_clean_iqr$price, main = "Price Distribution (AFTER IQR)")
boxplot(df_clean_iqr$price, main = "Price Boxplot (AFTER IQR)")


