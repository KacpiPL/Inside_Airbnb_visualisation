library(dplyr)
library(tidyr)
library(stringr)


rm(list=ls())

df <- read.csv("./data/df.csv")

##### NAs handling #####
# number of observations in each city
df %>%
  group_by(City) %>%
  summarise(n())

num_nas <- as.data.frame(colSums(is.na(df)))
# delete column with the most NAs and unnecessary
df <- subset(df, select = -neighbourhood_group)
# delete reviews per month column
df <- subset(df, select= -reviews_per_month)

df_without_nas <- na.omit(df)

df_without_nas %>%
  group_by(City) %>%
  summarise(n())

df <- df_without_nas
rm(df_without_nas)

##### Outliers handling #####

hist(df$price)


