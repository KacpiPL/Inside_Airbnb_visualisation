library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(sf)
library(reshape2)
library(hrbrthemes)
library(viridis)


rm(list=ls())
df <- read.csv("./data/df_final.csv")

# Inspirations:

# violin/boxplot <- price, rating
# 4 roomtype, city - barplot

# number reviews/rating
# scatterplot - price/rating, city/roomtype

# density/hist - lastreview
# powyżej 10 listings <- is business???

# heatmap

##### Graphs #####
df_Berlin <- df[df$City == "Berlin",]
df_Paris <- df[df$City == "Paris",]
df_London <- df[df$City == "London",]

par(mfrow=c(2,2))
hist(df_Berlin$Price_EUR)
hist(df_Paris$Price_EUR)
hist(df_London$Price_EUR)

dev.off()

# Violin plot
n_groupped <- df %>%
  group_by(City) %>%
  summarise(num=n())

df %>%
  left_join(n_groupped) %>%
  mutate(myaxis = paste0(City, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=Price_EUR, fill=City)) +
    geom_violin(width=1) +
    geom_boxplot(width=0.25, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Price vs City") +
  xlab("")


# Scatterplot overloaded
ggplot(df, aes(x=Rating, y=Price_EUR)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")

# Bubble chart <- find some interesting variables
# room_type
# price
# rating
# 

ggplot(df, aes(x = Rating, y = Price_EUR, size = Rating)) + 
  geom_point(alpha = 0.7) + 
  theme_light() +
  labs(title = "Price vs. Number of Reviews by Room Type", x = "Rating", y = "Price (EUR)")


# Boxplot
ggplot(data = df, aes(y = Price_EUR, x = City)) +
  geom_boxplot()



