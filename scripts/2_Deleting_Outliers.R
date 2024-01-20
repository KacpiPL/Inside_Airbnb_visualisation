library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(sf)
library(treemap)

rm(list=ls())

# Read .csv from script1
df <- read.csv("./data/df.csv")

##### NAs handling #####
# Calculate the number of observations in each city
num_obs_city_with_nas <- df %>%
  group_by(City) %>%
  summarise(n())

# Check the number of NAa and write it to df
num_nas <- as.data.frame(colSums(is.na(df)))

# Delete column with the most NAs and unnecessary
df <- subset(df, select = -neighbourhood_group)

# Delete reviews per month column
df <- subset(df, select= -reviews_per_month)

# Delete NAs
df_without_nas <- na.omit(df)

# Calculate the number of observations in each city after deleting NAs
num_obs_city_without_nas <- df_without_nas %>%
  group_by(City) %>%
  summarise(n())

# Overwrite df with df_without_nas
df <- df_without_nas

# Remove unnecessary df
rm(df_without_nas)

##### Outliers handling #####
# Delete price column - we have Price_EUR and 
# We do not want z_score method to take into consideration 2 columns with the same information
df <- subset(df, select = -price)

# In case of deleting outliers we need to delete them separately in each city
df_Berlin <- df[df$City == "Berlin",]
df_Paris <- df[df$City == "Paris",]
df_London <- df[df$City == "London",]

# function to delete outliers according to z_score method
delete_outliers <- function(df, threshold){
  
  # Find numeric columns
  num_columns <- sapply(df, is.numeric)
  
  # Create df with z-scores
  z_scores <- df[num_columns] %>% 
    lapply(function(x) scale(x, center = TRUE, scale = TRUE)) %>%
    as.data.frame()
  
  # Create a logical index for rows to keep
  rows_to_keep <- apply(z_scores, 1, function(x) all(abs(x) <= threshold))
  
  # Filter out outliers
  df <- df[rows_to_keep, ]
  
  return(df)
}

# Apply function to each city
df_Berlin_z <- delete_outliers(df_Berlin, 3)
df_Paris_z <- delete_outliers(df_Paris, 3)
df_London_z <- delete_outliers(df_London, 3)

# Merge dfs
df_z <- rbind(df_Berlin_z, df_Paris_z, df_London_z)

# Overwrite df with df_z
df <- df_z

# Calculate the number of observations after applying z score method
num_obs_city_without_nas_without_outliers <- df %>% 
  group_by(City) %>%
  summarise(n())

# According to the z score method applied to each column we lose around 400 thousand observations
# 648 433 <- n(df) at the beginning
# 233 451 <- n(df) after cleaning the df and outliers handling

# Write csv
write.csv(df, "./data/final_df.csv")
rm(list=ls())

df <- read.csv("./data/final_df.csv")

