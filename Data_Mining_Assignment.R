# STQD6414 Data Mining - Assignment 1
 
#-------------------------------------------
# Load library
#install.packages("arrow")

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
df_reduced <- df_merged%>%filter(date == "2025-01-01", 
                                 state == "Selangor")

# Change df_clean$item_group from character to factor
df_reduced$item_group <- as.factor(df_reduced$item_group)

# Change date format
df_reduced$date <- format(df_reduced$date, "%d/%m/%Y")

str(df_reduced)
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
# Plotting function
plotingHist <- function(data, title) {
  p_hist <- ggplot(data, aes(x = price, fill = item_group)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    facet_wrap(~ item_group, scales = "free") +
    labs(title = paste("Price Distribution by Item Group (", title, ")"),
         x = "Price (RM)", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p_hist)
}

plotingBox <- function(data, title) {
  p_box <- ggplot(data, aes(x = item_group, y = price, fill = item_group)) +
    geom_boxplot() +
    labs(title = paste("Price Boxplot by Item Group (", title, ")"),
         x = "Item Group", y = "Price (RM)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  print(p_box)
}

#-------------------------------
# Outliers treatment (BEFORE)
plotingHist(df_clean, "BEFORE")
plotingBox(df_clean,"BEFORE")

# IQR grouped by item_code
df_clean_iqr <- df_clean %>%
  group_by(item_code, item_group) %>%
  mutate(
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - (1.5 * IQR), 
    upper_bound = Q3 + (1.5 * IQR)
  ) %>%
  filter(price >= lower_bound & price <= upper_bound) %>%
  ungroup()

plotingHist(df_clean_iqr, "AFTER IQR")
plotingBox(df_clean_iqr, "AFTER IQR")

#-------------------------------------
# Median
df_clean_median <- df_clean %>%
  group_by(item_group) %>%
  mutate(
    med_price = median(price, na.rm = TRUE),
    abs_deviation = abs(price - med_price),
    mad_value = median(abs_deviation, na.rm = TRUE),
    threshold = ifelse(mad_value == 0, 0, abs_deviation / mad_value)
  ) %>%
  filter(threshold <= 3.0) %>% 
  ungroup()

plotingHist(df_clean_median, "AFTER Median")
plotingBox(df_clean_median, "AFTER Median")
