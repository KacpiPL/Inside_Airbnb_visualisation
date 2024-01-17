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

df <- read.csv("./data/final_df.csv")
df <- df[, -1]

df <- df %>%
  filter(Date == "2023-12-31")

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

dev.off()

# Violin plot + Boxplot
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

# Density Plot of Ratings with Facet by Room Type
ggplot(df, aes(x = Rating, fill = room_type)) + 
  geom_density(alpha = 0.7) + 
  facet_wrap(~ room_type) +
  theme_light() +
  labs(title = "Density of Ratings by Room Type", x = "Rating", y = "Density")

# Mean price by City, room type
mean_price_room_type <- df %>%
  group_by(City, room_type) %>%
  summarise(mean_price = mean(Price_EUR))

ggplot(mean_price_room_type, aes(x=City, y=mean_price, fill=room_type)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Mean Price rent price by City and Room Type", 
    x = "City", 
    y = "Mean Price (EUR)") +
  theme_light()


##### to be changed #####
# Scatterplot overloaded
ggplot(df_Berlin, aes(x=Rating, y=Price_EUR)) +
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

ggplot(df, aes(x = Rating, y = Price_EUR, size = Rating)) + 
  geom_point(alpha = 0.7) + 
  theme_light() +
  labs(title = "Price vs. Number of Reviews by Room Type", x = "Rating", y = "Price (EUR)")


##### Listings in time #####
rm(list=ls())

# read df from script1
df <- read.csv("./data/df.csv")

df_Berlin <- df[df$City == "Berlin",]
df_Paris <- df[df$City == "Paris",]
df_London <- df[df$City == "London",]

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
df <- df_z

rm(df_Berlin, df_Berlin_z, df_London, df_London_z, df_Paris, df_Paris_z, df_z)

df_chart <- df[, c("Date", "City", "Beds", "Price_EUR")]
df_chart <- na.omit(df_chart)

# Avg price for every city in time
df_chart_grouped <- df_chart %>%
  group_by(Date, City) %>%
  summarise(avg_price = round(mean(Price_EUR/Beds), 2))

ggplot(data = df_chart_grouped, aes(x = Date, y = avg_price, 
                          color = factor(City),
                          group = factor(City))) +
  geom_point() +
  geom_line() +
  theme_minimal()

# Number of listings in each city
df_chart2 <- df[, c("Date", "City")]
df_chart2 <- na.omit(df_chart2)

df_chart2_grouped <- df_chart2 %>%
  group_by(Date, City) %>%
  summarise(num_listings = round(n(), 2))

ggplot(data = df_chart2_grouped, aes(x = Date, y = num_listings, 
                                    color = factor(City),
                                    group = factor(City))) +
  geom_point() +
  geom_line() +
  theme_minimal()




