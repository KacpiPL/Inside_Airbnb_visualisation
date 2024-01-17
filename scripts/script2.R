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
# number of observations in each city
num_obs_city_with_nas <- df %>%
  group_by(City) %>%
  summarise(n())

num_nas <- as.data.frame(colSums(is.na(df)))

#df <- df %>%
  #mutate(neighbourhood = ifelse(City == "Berlin", neighbourhood_group, neighbourhood)) %>%
  #mutate(neighbourhood = ifelse(City == "Paris" & neighbourhood == "Entrepôt", "Enclos-St-Laurent", neighbourhood)) %>%
  #mutate(neighbourhood = ifelse(City == "Paris" & neighbourhood == "Buttes-Montmartre", "Butte-Montmartre", neighbourhood))

# Delete column with the most NAs and unnecessary
df <- subset(df, select = -neighbourhood_group)

# Delete reviews per month column
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

# According to the z score method applied to each column we lose around 400 thousand observations
# 648 433 <- n(df) at the beginning
# 234 833 <- n(df) after cleaning the df and outliers handling

write.csv(df, "./data/final_df.csv")
rm(list=ls())

df <- read.csv("./data/final_df.csv")

##### Charts #####
ggplot(data = df, aes(y = Price_EUR, x = City)) +
  geom_boxplot()

par(mfrow=c(2,2))
hist(df_Paris_z$Price_EUR)
hist(df_Berlin_z$Price_EUR)
hist(df_London_z$Price_EUR)

##### Scatterplot
df %>%
  filter(Date == '2023-12-31') %>%
    ggplot(aes(x = center_distance, y = Price_EUR, color = City)) +
      geom_point(alpha = 0.01) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Center Distance (km)", y = "Price (EUR)", color = "City") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      ggtitle("Price vs Center Distance by City")

##### Treemap

df_summary <- df %>%
  group_by(City, neighbourhood) %>%
  summarise(
    Listings = n(),
    Avg_Price = mean(Price_EUR, na.rm = TRUE)
  ) %>%
  mutate(
    Label = paste(neighbourhood, "\nAvg. Price:", round(Avg_Price, 2))
  )

# Define the color palette
pal <- viridis(256)

# Create a treemap for each city
for (city in unique(df_summary$City)) {
  df_city <- df_summary %>%
    filter(City == city)
  
  treemap(
    df_city,
    index = "Label",
    vSize = "Listings",
    vColor = "Avg_Price",
    type = "index",
    palette = pal,
    title = paste("Treemap of", city)
  )
}

##### Load the Shapefiles #####

# Load the shapefiles
berlin_sp <- geojson_read("./data/berlin.geojson", what = 'sp')
london_sp <- geojson_read("./data/london.geojson", what = 'sp')
paris_sp <- geojson_read("./data/paris.geojson", what = 'sp')

# Shapefile to Simple Feature objects
paris_sp_sf <- st_as_sf(paris_sp)
london_sp_sf <- st_as_sf(london_sp)
berlin_sp_sf <- st_as_sf(berlin_sp)

##### Map Chart #####

# Define the coordinates of the cities
Berlin_cent <- c(13.404954, 52.520008)
London_cent <- c(-0.118092, 51.509865)
Paris_cent  <- c(2.349014, 48.864716)


City_data <- data.frame(
  City = c("Berlin", "London", "Paris"),
  City_population = c(3769495, 8982000, 2148000), # Estimated population values
  City_Visitors = c(14000000, 19800000, 17800000) # Estimated visitor values
)

Berlin_cent <- c(13.404954, 52.520008)
London_cent <- c(-0.118092, 51.509865)
Paris_cent  <- c(2.349014, 48.864716)

# Create a dataframe of the cities
cities <- data.frame(
  City = c("Berlin", "London", "Paris"),
  Longitude = c(Berlin_cent[1], London_cent[1], Paris_cent[1]),
  Latitude = c(Berlin_cent[2], London_cent[2], Paris_cent[2])
)

# Plot the world map
maps::map('world', 
          col="#a6a6a6", fill=TRUE, bg="white", lwd=0.1,
          mar=rep(0,4),border=0, ylim=c(41,59), xlim=c(-10,22)
)
# Add the city dots to the map
points(x=cities$Longitude, y=cities$Latitude, col="slateblue", cex=3, pch=20)

# Calculate the great circle paths between the cities
inter_BL <- gcIntermediate(Berlin_cent, London_cent, n=50, addStartEnd=TRUE, breakAtDateLine=F)
inter_BP <- gcIntermediate(Berlin_cent, Paris_cent, n=50, addStartEnd=TRUE, breakAtDateLine=F)
inter_LP <- gcIntermediate(London_cent, Paris_cent, n=50, addStartEnd=TRUE, breakAtDateLine=F)

# Add the paths to the map
lines(inter_BL, col="slateblue", lwd=2)
lines(inter_BP, col="slateblue", lwd=2)
lines(inter_LP, col="slateblue", lwd=2)

text_info <- paste(
  "Cities:",
  paste(City_data$City, "Population:", City_data$Population, "Visitors:", City_data$Visitors, "Airbnb Listings:", City_data$AirbnbListings, sep=" ", collapse="\n")
)

# Add a text box to the plot
text(x=-10, y=59, labels=text_info, adj=c(0,1), cex=1.2)


# Join the simple feature objects with dfs
paris_data <- left_join(paris_sp_sf, df %>% filter(City == "Paris") %>% group_by(neighbourhood) %>% summarise(Avg_Rating = mean(Rating)), by = c("neighbourhood" = "neighbourhood"))
london_data <- left_join(london_sp_sf, df %>% filter(City == "London") %>% group_by(neighbourhood) %>% summarise(Avg_Rating = mean(Rating)), by = c("neighbourhood" = "neighbourhood"))
berlin_data <- left_join(berlin_sp_sf, df %>% filter(City == "Berlin") %>% group_by(neighbourhood) %>% summarise(Avg_Rating = mean(Rating)), by = c("neighbourhood" = "neighbourhood"))


centroids <- st_centroid(paris_data)

# Create the maps
colorFactorPal <- colorFactor("YlOrRd", paris_data$neighbourhood)
map <-  leaflet(paris_data) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(
    stroke = TRUE, 
    color = "white", 
    weight = "1", 
    smoothFactor = 0.3, 
    fillOpacity = 0.7, 
    fillColor = ~colorFactorPal(neighbourhood),
    label = ~neighbourhood
  ) %>%
  addLegend(
    pal = colorFactorPal, 
    values = ~neighbourhood, 
    title = "Neighbourhood",
    position = "bottomright"
  )


ggplot() +
  geom_sf(data = paris_data, aes(fill = Avg_Rating), color = "black") +
  scale_fill_continuous(low = "white", high = "red", name = "Avg. Rating")

ggplot() +
  geom_sf(data = london_data, aes(fill = Avg_Rating), color = "black") +
  scale_fill_continuous(low = "white", high = "red", name = "Avg. Rating")

ggplot() +
  geom_sf(data = berlin_data, aes(fill = Avg_Rating), color = "black") +
  scale_fill_continuous(low = "white", high = "red", name = "Avg. Rating")

##### Map Chart - Points #####


paris_data <- df %>% filter(City == "Paris")
london_data <- df %>% filter(City == "London")
berlin_data <- df %>% filter(City == "Berlin")

pal <- colorNumeric(palette = "viridis", domain = df$Price_EUR)

# Create the maps
leaflet(paris_data) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(Price_EUR), fillOpacity = 0.1, radius = 0.1) %>%
  setView(lng = mean(paris_data$longitude), lat = mean(paris_data$latitude), zoom = 12) %>%
  addLegend(pal = pal, values = ~Price_EUR, title = "Price")

leaflet(london_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    color = ~pal(Price_EUR),
    fillOpacity = 0.1,
    radius = 0.01  # Set the radius to a constant value
  ) %>%
  setView(lng = mean(london_data$longitude), lat = mean(london_data$latitude), zoom = 10) %>%
  addLegend(pal = pal, values = ~Price_EUR, title = "Price")

leaflet(berlin_data) %>%
  addCircles(~longitude, ~latitude, color = ~pal(Price_EUR), fillOpacity = 0.1, radius = 0.1) %>%
  setView(lng = mean(berlin_data$longitude), lat = mean(berlin_data$latitude), zoom = 12) %>%
  addLegend(pal = pal, values = ~Price_EUR, title = "Price") %>%
  addProviderTiles('Stadia.AlidadeSmooth') %>%
  addPolygons(data = berlin_sp_sf, color = ~pal(neighbourhood), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

pal <- colorFactor(palette = "viridis", domain = berlin_sp_sf$name)

# Create the map
leaflet(berlin_sp_sf) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(name), fillOpacity = 0.5, color = "white", weight = 1)
