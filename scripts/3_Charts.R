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
library(extrafont)
library(magick)

rm(list=ls())
# font_import()

# Read data
df <- read.csv("./data/final_df.csv")
df <- df[, -1]

# Filter data to have only the last date
df <- df %>%
  filter(Date == "2023-12-31")

##### Graphs #####
df_Berlin <- df[df$City == "Berlin",]
df_Paris <- df[df$City == "Paris",]
df_London <- df[df$City == "London",]

dev.off()

##### Violin plot price #####
# Prepare data
n_groupped <- df %>%
  group_by(City) %>%
  summarise(num=n(), mean_price = round(mean(Price_EUR),2))

# Chart
df %>%
  left_join(n_groupped) %>%
  mutate(myaxis = paste0(City, "\n", "n = ", num, "\n", "mean = ", mean_price)) %>%
  ggplot(aes(x=myaxis, y=Price_EUR, fill=City)) +
    geom_violin(width=1) +
    geom_boxplot(width=0.25, color="cornsilk4", alpha=0.2) + # cornsilk4
  scale_fill_brewer(palette="RdYlGn") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f2ebe6"),
    panel.background = element_rect(fill = "#f2ebe6"), 
    text = element_text(family = "Lora"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14)) +
   # axis.line = element_line(color = "grey")
  ggtitle("Price distributions across cities [EUR]") +
  xlab("") +
  ylab("Price")

##### Violin plot ratings distributions #####
n_groupped <- df %>%
  group_by(City) %>%
  summarise(num=n(), mean_price = round(mean(Rating),2))

df %>%
  left_join(n_groupped) %>%
  mutate(myaxis = paste0(City, "\n", "n = ", num, "\n", "mean = ", mean_price)) %>%
  ggplot(aes(x=myaxis, y=Rating, fill=City)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.25, color="cornsilk4", alpha=0.2) + # cornsilk4
  scale_fill_brewer(palette="RdYlGn") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f2ebe6"),
    panel.background = element_rect(fill = "#f2ebe6"), 
    text = element_text(family = "Lora"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14)) +
  # axis.line = element_line(color = "grey")
  ggtitle("Rating distributions across cities") +
  xlab("") +
  ylab("Rating")

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
    plot.background = element_rect(fill = "#f2ebe6"),
    panel.background = element_rect(fill = "#f2ebe6"), 
    text = element_text(family = "Lora"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14),
    legend.background = element_rect(fill = "#f2ebe6", size = 0.5),
    legend.key = element_rect(fill = "#f2ebe6", color = "black")) +
  ggtitle("Mean rent price per bed per city") +
  xlab("City") +
  ylab("Mean price/bed in EUR") +
  scale_fill_discrete(name = "Room type")

##### Bubble graph #####
# Prepare data
df_Bubble <- df %>%
  group_by(City, neighbourhood) %>%
  summarise(mean_center_distance = mean(center_distance),
            mean_price = mean(Price_EUR/Beds))

palette <- brewer.pal(11, "RdYlGn")
selected_colors <- palette[c(2, 4, 8)]

# Graph
ggplot(df_Bubble, aes(x = mean_center_distance, y = mean_price, colour = City)) +
  geom_point(alpha=0.7, size = 5) +
  # scale_color_brewer(palette="RdYlGn") +
  scale_color_manual(values = selected_colors) +
  theme(
    plot.background = element_rect(fill = "#f2ebe6"),
    panel.background = element_rect(fill = "#f2ebe6"), 
    text = element_text(family = "Lora"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14),
    legend.background = element_rect(fill = "#f2ebe6", size = 0.5),
    legend.key = element_rect(fill = "#f2ebe6", color = "black")) +
  ggtitle("Mean rent price vs mean distance from center") +
  xlab("Mean distance from city center") +
  ylab("Mean price/bed in EUR")

##### Listings in time #####
# In this part we want to have the number of all listings
# Because we use just the number of listings and price and bed columns
# We do not need to delete NAs from all columns
# We read the data from 1_Data_Processing once again

rm(list=ls())

# read df from 1_Data_Processing
df <- read.csv("./data/df.csv")

df_Berlin <- df[df$City == "Berlin",]
df_Paris <- df[df$City == "Paris",]
df_London <- df[df$City == "London",]

# Function to delete outliers basing on z score method, but applied to just one column
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
# Data
df_chart_grouped <- df_chart %>%
  group_by(Date, City) %>%
  summarise(avg_price = round(mean(Price_EUR/Beds), 2))

# Graph
p1 <- ggplot(data = df_chart_grouped, aes(x = Date, y = avg_price, 
                          color = factor(City),
                          group = factor(City))) +
  geom_point(size=2.5) +
  geom_line(size=0.75) +
  theme(
    plot.background = element_rect(fill = "#f2ebe6"),
    panel.background = element_rect(fill = "#f2ebe6"), 
    text = element_text(family = "Lora"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14),
    legend.background = element_rect(fill = "#f2ebe6", size = 0.5),
    legend.key = element_rect(fill = "#f2ebe6", color = "black")) +
  ggtitle("Mean price per bed in each city") +
  xlab("Date") +
  ylab("Mean price/bed in EUR") +
  labs(color = "City")

p1

# Number of listings in each city
df_chart2 <- df[, c("Date", "City")]
df_chart2 <- na.omit(df_chart2)

df_chart2_grouped <- df_chart2 %>%
  group_by(Date, City) %>%
  summarise(num_listings = round(n(), 2))

p2 <- ggplot(data = df_chart2_grouped, aes(x = Date, y = num_listings, 
                                    color = factor(City),
                                    group = factor(City))) +
  geom_point(size=2.5) +
  geom_line(size=0.75) +
  theme(
    plot.background = element_rect(fill = "#f2ebe6"),
    panel.background = element_rect(fill = "#f2ebe6"), 
    text = element_text(family = "Lora"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(size=22,
                              face="bold"),
    axis.title = element_text(size=14),
    axis.text.x = element_text(size=14),
    legend.background = element_rect(fill = "#f2ebe6", size = 0.5),
    legend.key = element_rect(fill = "#f2ebe6", color = "black")) +
  ggtitle("Total number of listings") +
  xlab("Date") +
  ylab("Number of listings") +
  labs(color = "City")

p1
p2

##### Visualization of Top host names [on a full dataset, before deleting the column] ####
top_hosts <- df %>%
  count(City, host_name) %>%
  group_by(City) %>%
  top_n(5, n) %>%
  ungroup()

# Create the histograms
ggplot(top_hosts, aes(x = host_name, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ City, scales = "free") +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14, angle = 45, hjust = 1),
        axis.text.y = element_text(size=14),
        strip.text = element_text(size = 16),  # Increase the size of facet labels
        plot.background = element_rect(fill = "#f2ebe6"),
        panel.background = element_rect(fill = "#f2ebe6"), 
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(family = "Lora"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "#f2ebe6", size = 0.2),
        legend.position = c(1, 1),
        legend.justification = c(1, 0),
        legend.key = element_rect(fill = "#f2ebe6", color = "black"),
        plot.margin = margin(20, 10, 10, 10)) +
  xlab("Host Name") +
  ylab("Frequency") +
  ggtitle("Top 5 Host Names in Each City")
##