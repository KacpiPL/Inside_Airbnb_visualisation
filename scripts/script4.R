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


##### MAPS #####
# Load the shapefiles
berlin_sp <- geojson_read("./data/berlin.geojson", what = 'sp')
london_sp <- geojson_read("./data/london.geojson", what = 'sp')
paris_sp <- geojson_read("./data/paris.geojson", what = 'sp')

# Shapefile to Simple Feature objects
paris_sp_sf <- st_as_sf(paris_sp)
london_sp_sf <- st_as_sf(london_sp)
berlin_sp_sf <- st_as_sf(berlin_sp)

# Join the data frames
paris_map <- left_join(paris_sp_sf, 
                       df %>% filter(City == "Paris") %>% 
                         group_by(neighbourhood) %>% 
                         summarise(Avg_Rating = mean(Rating), Avg_Price = mean(Price_EUR/Beds)) %>% 
                         mutate(City = "Paris"), 
                       by = c("neighbourhood" = "neighbourhood"))

london_map <- left_join(london_sp_sf, 
                        df %>% filter(City == "London") %>% 
                          group_by(neighbourhood) %>% 
                          summarise(Avg_Rating = mean(Rating), Avg_Price = mean(Price_EUR/Beds)) %>% 
                          mutate(City = "London"), 
                        by = c("neighbourhood" = "neighbourhood"))

berlin_map <- left_join(berlin_sp_sf, 
                        df %>% filter(City == "Berlin") %>% 
                          group_by(neighbourhood) %>% 
                          summarise(Avg_Rating = mean(Rating), Avg_Price = mean(Price_EUR/Beds), cnt = n()) %>% 
                          filter(cnt >10) %>%
                          mutate(City = "Berlin"), 
                        by = c("neighbourhood" = "neighbourhood"))
  
  
  
  df %>%
  filter(City == "Berlin") %>%
  group_by(neighbourhood) %>%
  summarise(Avg_Rating = mean(Rating), Avg_Price = mean(Price_EUR/Beds), cnt = n()) %>%
  filter(cnt > 10) %>%
  mutate(City = "Berlin") %>%
  right_join(berlin_sp_sf, by = c("neighbourhood" = "neighbourhood")) %>%
  select(-cnt)

# Make the geometries valid
paris_map$geometry <- st_make_valid(paris_map$geometry)
london_map$geometry <- st_make_valid(london_map$geometry)
berlin_map$geometry <- st_make_valid(berlin_map$geometry)

# Create a list of the city maps
city_maps <- list(Paris = paris_map, London = london_map, Berlin = berlin_map)

# Initialize an empty data frame to store the results
min_max_prices <- data.frame()

# For each city, find the neighborhood with the highest and lowest average prices
min_prices <- list()
max_prices <- list()

# For each city, find the neighborhood with the highest and lowest average prices
for(city in names(city_maps)) {
  city_data <- city_maps[[city]]
  max_price <- city_data[which.max(city_data$Avg_Price), ]
  max_price$Value <- "max"
  max_prices[[city]] <- max_price
  min_price <- city_data[which.min(city_data$Avg_Price), ]
  min_price$Value <- "min"
  min_prices[[city]] <- min_price
}

for(city in names(city_maps)) {
  city_data <- city_maps[[city]]
  assign(paste0("max_", city), city_data[which.max(city_data$Avg_Price), ])
  assign(paste0("min_", city), city_data[which.min(city_data$Avg_Price), ])
}

### LONDON MAP
ggplot(data = london_map) +
  geom_sf(aes(fill = Avg_Price)) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), na.value = "antiquewhite1") +
  geom_text_repel(data = max_London, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  fontface = "bold", nudge_x = -0.24, nudge_y = 0.18) +
  geom_text_repel(data = min_London, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  fontface = "bold", nudge_x = -.23, nudge_y = -0.05) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("London", subtitle = "Average prices per night [EUR]") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "White", color = "black", size = 0.5),
        legend.key = element_rect(fill = "white", color = "black")) +
  labs(fill = "Average Price/Bed") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

### BERLIN MAP
ggplot(data = berlin_map) +
  geom_sf(aes(fill = Avg_Price)) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), na.value = "antiquewhite1") +
  geom_text_repel(data = max_Berlin, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  fontface = "bold", nudge_x = -0.35, nudge_y = -0.14) +
  geom_text_repel(data = min_Berlin, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  fontface = "bold", nudge_x = .23, nudge_y = 0.03) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Berlin", subtitle = "Average prices per night [EUR]") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "White", color = "black", size = 0.5),
        legend.key = element_rect(fill = "white", color = "black")) +
  labs(fill = "Average Price/Bed") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

### PARIS MAP
ggplot(data = paris_map) +
  geom_sf(aes(fill = Avg_Price)) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), na.value = "antiquewhite1") +
  geom_text_repel(data = max_Paris, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  fontface = "bold", nudge_x = -0.07, nudge_y = 0.04) +
  geom_text_repel(data = min_Paris, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  fontface = "bold", nudge_x = .07, nudge_y = 0.02) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Paris", subtitle = "Average prices per Bed [EUR]") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "White", color = "black", size = 0.5),
        legend.key = element_rect(fill = "white", color = "black")) +
  labs(fill = "Average Price/Bed") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

