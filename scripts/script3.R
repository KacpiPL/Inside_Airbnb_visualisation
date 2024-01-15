library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(sf)

rm(list=ls())
df <- read.csv("./data/df_final.csv")

# violin/boxplot <- price, rating
# 4 roomtype, city - barplot

# number reviews/rating
# scatterplot - price/rating, city/roomtype

# density/hist - lastreview
# powyżej 10 listings <- is business???

# heatmap
avg_ratings <- aggregate(Rating ~ City + Type, data = df, mean)



ggplot(data = df, aes(y = Price_EUR, x = City)) +
  geom_boxplot()

unique(df$room_type)


par(mfrow=c(2,2))
hist(df_Paris_z$Price_EUR)
hist(df_Berlin_z$Price_EUR)
hist(df_London_z$Price_EUR)