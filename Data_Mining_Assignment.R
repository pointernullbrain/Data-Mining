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

#-------------------------------------------
# Data cleaning
# Remove duplicates (if any)
sum(duplicated(df_reduced))
df_reduced <- df_reduced %>% distinct()

# Check missing values in raw records
sum(is.na(df_reduced))
colSums(is.na(df_reduced))

df_clean <- na.omit(df_reduced)
df_clean_selective <- df_reduced %>% drop_na(item_group)

# Check again duplicates and missing values
sum(duplicated(df_clean))
sum(is.na(df_clean))
colSums(is.na(df_clean))

# Check for duplicates and missing values for second method to handle missing values
sum(duplicated(df_clean_selective))
sum(is.na(df_clean_selective))
colSums(is.na(df_clean_selective))

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

#create factors for item group and item category
distinct(df_clean$item_group)
df_clean <- df_clean %>% 
  mutate(item_group_factor = as.factor(item_group)) %>%
  mutate(item_category_factor = as.factor(item_category))
summary(df_clean$item_group_factor)

## checking validity
par(mfrow = c(1,1))
plot(df_clean$item_group_factor)  
levels(df_clean$item_group_factor)

#log transform
ggplot(df_clean, aes(x = item_group_factor, y = price, fill = item_group_factor)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +
  scale_y_log10(labels = scales::dollar_format(prefix = "RM ")) + # Converts y-axis to log scale
  labs(
    title = "Log-Transformed Price Distribution by Item Group",
    x = "Item Group",
    y = "Price (MYR - Log10 Scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotates long text labels
    legend.position = "none"
  )





