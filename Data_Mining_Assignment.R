# STQD6414 Data Mining - Assignment 1

#-------------------------------------------
# Load library
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
#create factors for item group and item category
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

#------------------------------------
# Summary
summary(df_clean_iqr)
summary(df_clean_median)
summary(df_clean_selective)

#------------------------------------
# Pipeline Summary
cat("\n ***** Pipeline Summary ***** \n",
    "Filtered Rows (Selangor, 1/1/2025: ", nrow(df_reduced), "\n",
    "Rows After Missing Value Removal: ", nrow(df_clean), "\n",
    "Rows After IQR: ", nrow(df_clean_iqr), "\n",
    "Rows After Median: ", nrow(df_clean_median), "\n" )

# Export cleaned dataset
df_final <- df_clean_median
write.csv(df_final, "final_cleaned_pricecatcher_selangor.csv",
          row.names = FALSE)
