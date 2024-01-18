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
library(gridExtra)
library(RColorBrewer)

rm(list=ls())

# (brewer.pal(9, "RdYlGn"))
# RColorBrewer

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

##### Violin plot + Boxplot #####
# Prepare data
n_groupped <- df %>%
  group_by(City) %>%
  summarise(num=n())

# Chart
df %>%
  left_join(n_groupped) %>%
  mutate(myaxis = paste0(City, "\n", "n = ", num)) %>%
  ggplot(aes(x=myaxis, y=Price_EUR, fill=City)) +
    geom_violin(width=1) +
    geom_boxplot(width=0.25, color="cornsilk4", alpha=0.2) + # cornsilk4
  scale_fill_brewer(palette="RdYlGn") +
  theme(
    legend.position="none",
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey",
                                    linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey",
                                    linewidth = 0.15)
   # axis.line = element_line(color = "grey")
  ) +
  ggtitle("Price distributions across cities") +
  xlab("") +
  ylab("Price in EUR")

##### Mean price by City, room type #####
# Prepare Data
mean_price_room_type <- df %>%
  group_by(City, room_type) %>%
  summarise(mean_price = mean(Price_EUR/Beds), .groups = 'drop')

### Define the new order of columns to be shown
desired_order <- c("Hotel room", "Entire home/apt", "Private room", "Shared room")
### Reorder room_type in mean_price_room_type dataset
mean_price_room_type$room_type <- factor(mean_price_room_type$room_type, levels = desired_order)

# Chart
ggplot(mean_price_room_type, aes(x=City, y=mean_price, fill=room_type)) +
  geom_col(position = position_dodge()) + # Create a bar plot
  scale_fill_brewer(palette="RdYlGn") +
  theme(
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=12),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey",
                                    linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey",
                                    linewidth = 0.15)
  ) +
  ggtitle("Mean rent price per bed per city") +
  xlab("City") +
  ylab("Mean price/bed in EUR")

##### to be changed #####

ggplot(df, aes(x = center_distance, y = Price_EUR/Beds)) +
  geom_point() +
  labs(title = "Price vs. Distance from City Center", x = "Distance from Center (km)", y = "Price (EUR)")


# TEN ZOSTAJE
ggplot(df_Bubble, aes(x = mean_center_distance, y = mean_price, colour = City)) +
  geom_point(alpha=0.7, size = 7)

# Histograms of ratings

ggplot(df_Berlin, aes(x = Rating)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Distribution of Property Ratings", x = "Rating", y = "Count")

ggplot(df_London, aes(x = Rating)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Distribution of Property Ratings", x = "Rating", y = "Count")

ggplot(df_Paris, aes(x = Rating)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Distribution of Property Ratings", x = "Rating", y = "Count")


# combined histograms - popracować wyskalować
ggplot(df, aes(x = Rating, fill = City)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Ratings across Countries", x = "Rating", y = "Frequency")


df_hist <- df %>%
  group_by(City, Rating) %>%
  summarise(count = n()) %>%
  mutate(Percent = count / sum(count))

ggplot(df_hist, aes(x = Rating, y = Percent, fill = City)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Percentage Share of Ratings by City", x = "Ratings", y = "Percentage") +


df %>%
  ggplot(aes(x=Rating, fill=City)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


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

p1 <- ggplot(data = df_chart_grouped, aes(x = Date, y = avg_price, 
                          color = factor(City),
                          group = factor(City))) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Average Price per Bed in Each City")

p1

# Number of listings in each city
# Change to
df_chart2 <- df[, c("Date", "City")]
df_chart2 <- na.omit(df_chart2)

df_chart2_grouped <- df_chart2 %>%
  group_by(Date, City) %>%
  summarise(num_listings = round(n(), 2))

p2 <- ggplot(data = df_chart2_grouped, aes(x = Date, y = num_listings, 
                                    color = factor(City),
                                    group = factor(City))) +
  geom_point() +
  geom_line() +
  theme_minimal() + 
  labs(title = "Total number of listings")
p2

grid.arrange(p2, p1, ncol=1)


##### BACKUP #####
# Density Plot of Ratings with Facet by Room Type
ggplot(df, aes(x = Rating, fill = room_type)) + 
  geom_density(alpha = 0.7) + 
  facet_wrap(~ room_type) +
  theme_light() +
  labs(title = "Density of Ratings by Room Type", x = "Rating", y = "Density")


# Scatterplot overloaded
ggplot(df_Berlin, aes(x=Rating, y=Price_EUR/Beds)) +
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

