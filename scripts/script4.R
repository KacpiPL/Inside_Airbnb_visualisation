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
library(RColorBrewer)
library(ggrepel)
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
  max_price <- city_data[which.max(city_data$Avg_Rating), ]
  max_price$Value <- "max"
  max_prices[[city]] <- max_price
  min_price <- city_data[which.min(city_data$Avg_Rating), ]
  min_price$Value <- "min"
  min_prices[[city]] <- min_price
}

for(city in names(city_maps)) {
  city_data <- city_maps[[city]]
  assign(paste0("max_price_", city), city_data[which.max(city_data$Avg_Price), ])
  assign(paste0("min_price_", city), city_data[which.min(city_data$Avg_Price), ])
  assign(paste0("max_rating_", city), city_data[which.max(city_data$Avg_Rating), ])
  assign(paste0("min_rating_", city), city_data[which.min(city_data$Avg_Rating), ])
}

### LONDON MAP PRICE
ggplot(data = london_map) +
  geom_sf(aes(fill = Avg_Price)) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), 
                       na.value = "antiquewhite1", 
                       guide = guide_colourbar(direction = "horizontal")) +
  geom_text_repel(data = max_price_London, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -0.24, nudge_y = 0.18) +
  geom_text_repel(data = min_price_London, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -.23, nudge_y = -0.05) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle('Average Price/Bed in London') +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),  # Add 20 pixels of space below the title
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
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
  labs(fill = "Average Price") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

### LONDON MAP RATING
ggplot(data = london_map) +
  geom_sf(aes(fill = Avg_Rating)) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdYlGn"), na.value = "antiquewhite1", 
                       guide = guide_colourbar(direction = "horizontal")) +
  geom_text_repel(data = max_rating_London, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = 0.14, nudge_y = 0.08) +
  geom_text_repel(data = min_rating_London, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -.23, nudge_y = 0.18) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle('Average Rating in London') +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),  # Add 20 pixels of space below the title
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.background = element_rect(fill = "#f2ebe6"),
        panel.background = element_rect(fill = "#f2ebe6"), 
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(family = "Lora"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "#f2ebe6", size = 0.2),
        legend.position = c(1, 1),
        legend.justification = c(1, 0),
        legend.key = element_rect(fill = "#f2ebe6", color = "black"),
        plot.margin = margin(20, 20, 10, 10)) +
  labs(fill = "Average Rating") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

### Berlin MAP PRICE
ggplot(data = berlin_map) +
  geom_sf(aes(fill = Avg_Price)) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), na.value = "antiquewhite1", 
                       guide = guide_colourbar(direction = "horizontal")) +
  geom_text_repel(data = max_price_Berlin, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -0.24, nudge_y = -0.15) +
  geom_text_repel(data = min_price_Berlin, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = .23, nudge_y = 0.05) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle('Average Price/Bed in Berlin') +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),  # Add 20 pixels of space below the title
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.background = element_rect(fill = "#f2ebe6"),
        panel.background = element_rect(fill = "#f2ebe6"), 
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(family = "Lora"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "#f2ebe6", size = 0.2),
        legend.position = c(1, 1),
        legend.justification = c(1, 0),
        legend.key = element_rect(fill = "#f2ebe6", color = "black"),
        plot.margin = margin(20, 20, 10, 10)) +
  labs(fill = "Average Price") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

### Berlin MAP RATING
ggplot(data = berlin_map) +
  geom_sf(aes(fill = Avg_Rating)) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdYlGn"), na.value = "antiquewhite1", 
                       guide = guide_colourbar(direction = "horizontal")) +
  geom_text_repel(data = max_rating_Berlin, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = 0.2, nudge_y = 0.06) +
  geom_text_repel(data = min_rating_Berlin, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -.13, nudge_y = 0.08) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle('Average Rating in Berlin') +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),  # Add 20 pixels of space below the title
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.background = element_rect(fill = "#f2ebe6"),
        panel.background = element_rect(fill = "#f2ebe6"), 
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(family = "Lora"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "#f2ebe6", size = 0.2),
        legend.position = c(1, 1),
        legend.justification = c(1, 0),
        legend.key = element_rect(fill = "#f2ebe6", color = "black"),
        plot.margin = margin(20, 20, 10, 10)) +
  labs(fill = "Average Rating") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))



### PARIS MAP PRICE
ggplot(data = paris_map) +
  geom_sf(aes(fill = Avg_Price)) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), na.value = "antiquewhite1", 
                       guide = guide_colourbar(direction = "horizontal")) +
  geom_text_repel(data = max_price_Paris, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -0.04, nudge_y = -0.05) +
  geom_text_repel(data = min_price_Paris, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = .03, nudge_y = 0.02) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle('Average Price/Bed in Paris') +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),  # Add 20 pixels of space below the title
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.background = element_rect(fill = "#f2ebe6"),
        panel.background = element_rect(fill = "#f2ebe6"), 
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(family = "Lora"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "#f2ebe6", size = 0.2),
        legend.position = c(1, 1),
        legend.justification = c(1, 0),
        legend.key = element_rect(fill = "#f2ebe6", color = "black"),
        plot.margin = margin(20, 20, 10, 10)) +
  labs(fill = "Average Rating") +
  labs(fill = "Average Price") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

### PARIS MAP RATING
ggplot(data = paris_map) +
  geom_sf(aes(fill = Avg_Rating)) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdYlGn"), na.value = "antiquewhite1", 
                       guide = guide_colourbar(direction = "horizontal")) +
  geom_text_repel(data = max_rating_Paris, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = 0.03, nudge_y = 0.03) +
  geom_text_repel(data = min_rating_Paris, 
                  aes(label = neighbourhood, 
                      x = st_coordinates(st_centroid(geometry))[,1], 
                      y = st_coordinates(st_centroid(geometry))[,2]), 
                  family = 'Lora', nudge_x = -.03, nudge_y = 0.03) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle('Average Rating in Paris') +
  theme(plot.title = element_text(size=18, face="bold", margin = margin(b = 12)),  # Add 20 pixels of space below the title
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.background = element_rect(fill = "#f2ebe6"),
        panel.background = element_rect(fill = "#f2ebe6"), 
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(family = "Lora"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.background = element_rect(fill = "#f2ebe6", size = 0.2),
        legend.position = c(1, 1),
        legend.justification = c(1, 0),
        legend.key = element_rect(fill = "#f2ebe6", color = "black"),
        plot.margin = margin(20, 20, 10, 10)) +
  labs(fill = "Average Rating") +
  labs(fill = "Average Rating") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "° E")) +
  scale_y_continuous(labels = function(y) paste0(round(y, 1), "° N"))

##### Leaflet

lat <- 51.1657
lng <- 10.4515

berlin <- c(52.5200, 13.4050)
london <- c(51.5074, -0.1278)
paris <- c(48.8566, 2.3522)

# Create a leaflet map
m <- leaflet() %>%
  setView(lng = lng, lat = lat, zoom = 4) %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
           options = tileOptions(minZoom = 3, maxZoom = 10)) %>%
  addMarkers(lng = berlin[2], lat = berlin[1], popup = "Berlin") %>%
  addMarkers(lng = london[2], lat = london[1], popup = "London") %>%
  addMarkers(lng = paris[2], lat = paris[1], popup = "Paris") %>%
  addControl("Leaflet Map", position = "bottomright")



# Plot the points on the map
ggmap(london_map) +
  geom_point(data = london_df, aes(x = longitude, y = latitude, color = as.factor(Business_Owned)), alpha = 0.3, size = 0.05) +
  scale_color_manual(values = c("#a6cee3", "red")) +
  labs(x = "Longitude", y = "Latitude", color = "Business Owned") +
  ggtitle("Occurrences in London")
