library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


rm(list=ls())

df <- read.csv("./data/df.csv")

##### NAs handling #####
# number of observations in each city
num_obs_city_with_nas <- df %>%
  group_by(City) %>%
  summarise(n())

num_nas <- as.data.frame(colSums(is.na(df)))
# delete column with the most NAs and unnecessary
df <- subset(df, select = -neighbourhood_group)
# delete reviews per month column
df <- subset(df, select= -reviews_per_month)

df_without_nas <- na.omit(df)

num_obs_city_without_nas <- df_without_nas %>%
  group_by(City) %>%
  summarise(n())

df <- df_without_nas
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

df <- df_z

num_obs_city_without_nas_without_outliers <- df %>% 
  group_by(City) %>%
  summarise(n())

# According to the z score method applied to each column we lose another 20 thousand observations

##### Apply z score method to just one column - Price_EUR #####
# Run code from NAs handling before and divide into 3 dfs

delete_outliers_one_column <- function(df, threshold, col_name){
  z_scores <- df[col_name] %>% 
    lapply(function(x) scale(x, center = TRUE, scale = TRUE)) %>%
    as.data.frame()
  
  # Create a logical index for rows to keep
  threshold <- 3
  rows_to_keep <- apply(z_scores, 1, function(x) all(abs(x) <= threshold))
  
  # Filter out outliers
  df <- df[rows_to_keep, ]
}

# Apply function to each city
df_Berlin_z <- delete_outliers_one_column(df_Berlin, 3, "Price_EUR")
df_Paris_z <- delete_outliers_one_column(df_Paris, 3, "Price_EUR")
df_London_z <- delete_outliers_one_column(df_London, 3, "Price_EUR")

# Merge dfs
df_z <- rbind(df_Berlin_z, df_Paris_z, df_London_z)

df_z %>% 
  group_by(City) %>%
  summarise(n())

##### Charts #####
ggplot(data = df, aes(y = Price_EUR, x = City)) +
  geom_boxplot()

par(mfrow=c(2,2))
hist(df_Paris_z$Price_EUR)
hist(df_Berlin_z$Price_EUR)
hist(df_London_z$Price_EUR)




