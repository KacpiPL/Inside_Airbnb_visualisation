library(dplyr)
library(tidyr)
library(stringr)
library(geosphere)
library(purrr)
library(readr)
rm(list=ls())

# Load all datasets
file_paths_berlin <- paste0("./data/Berlin", 1:4, "_listings.csv")
file_paths_london <- paste0("./data/London", 1:4, "_listings.csv")
file_paths_paris <- paste0("./data/Paris", 1:4, "_listings.csv")

# Defining the quarter end dates
quarter_end_dates <- c("2023-03-31", "2023-06-30", "2023-09-30", "2023-12-31")

# Function to read and combine data for a city
read_and_combine <- function(file_paths, city, currency) {
  map_df(file_paths, ~ {
    data <- read_csv(.x)
    data <- data %>%
      mutate(
        City = city,
        Date = quarter_end_dates[str_extract(.x, "(?<=Berlin|London|Paris)\\d") %>% as.integer()],
        Currency = currency
      )
    data
  })
}

# Read and combine the data for each city
df_berlin <- read_and_combine(file_paths_berlin, "Berlin", "EUR")
df_london <- read_and_combine(file_paths_london, "London", "GBP")
df_paris <- read_and_combine(file_paths_paris, "Paris", "EUR")

# Join the df's into one df
df <- rbind(df_berlin, df_london, df_paris)
rm(df_berlin, df_london, df_paris)

# Change the order of columns
for (i in 1:2){
  column_names <- names(df)[-ncol(df)]
  # Inserting the last column's name at the second position
  new_order <- c(column_names[1], names(df)[ncol(df)], column_names[-1])
  # Reordering the dataframe columns
  df <- df[, new_order]
}

# Remove unnecessary variables
rm(i, column_names, new_order)

# Remove unnecessary columns
unnecessary_columns <- c("host_id", "host_name", "license")
df <- df[ , !names(df) %in% unnecessary_columns]

# Divide the column "name" to a few columns:
## Type
## Rating
## Bedroom
## Beds
## Baths

# Check the variable "name"
name <- df[500000, "name"]
name

# Separate the data - extract data from the column "name"
df1 <- df %>%
  mutate(
    Type = str_extract(name, "^[^·]+"),
    Rating = str_extract(name, "\\★[^·]+"),
    Bedroom = str_extract(name, "(?i)(\\d+ bedrooms?|studio)"),        # sometimes instead of bedroom there is "Studio".  "\\d+ (bedroom(s)? | Studio)"
    Beds = str_extract(name, "\\d+ bed(s)?\\b"),
    Bath = str_extract(name, "\\d*(\\.\\d+)?\\s*(private|shared)?\\s*(half-)?bath|Shared half-bath|Half-bath")    # \\d+(\\.\\d+)?\\s+(private|shared)?\\s*bath
  )

# Check if regex works correctly
rating_null <- df1[is.na(df1$Rating), ]
bedroom_null <- df1[is.na(df1$Bedroom), ]
beds_null <- df1[is.na(df1$Beds), ]
bath_null <- df1[is.na(df1$Bath), ]

df <- df1
# Remove unnecessary variables
rm(rating_null, bedroom_null, beds_null, bath_null, name, unnecessary_columns)

# Clean columns Rating, Bedroom, Beds, Bath
df$Rating <- as.numeric(gsub("★", "", df$Rating))

## Divide Bedroom to 2 columns -> Bedroom and is_studio
df$is_studio <- ifelse(df$Bedroom == "Studio", 1, 0)
df$Bedroom<- as.numeric(ifelse(df$Bedroom == "Studio", 1, gsub( " .*$", "", df$Bedroom)))

df$Beds <- as.numeric(gsub( " .*$", "", df$Beds))

# Bath column has also values like " half-bath", "Shared half-bath" and "Half-bath"
# We transform them as 0.5
df$Bath <- as.numeric(ifelse(
  df$Bath == " half-bath" | df$Bath == "Shared half-bath" | df$Bath == "Half-bath",
  0.5,
  gsub( " .*$", "", df$Bath)))

# Add binary column Half_bath
df$Half_bath <- ifelse(df$Bath %% 1 == 0.5, 1, 0)

# Round up the column Bath
df$Bath <- ceiling(df$Bath)

# Delete column "name"
df <- df[ , !names(df) %in% "name"]

# Add column Price_EUR
## GBP/EUR at the beginning of December = 1.16
df$Price_EUR <- ifelse(df$Currency == "EUR", df$price, df$price / 1.16)

# Define city centers longitude and latitude
Berlin_cent <- c(13.404954, 52.520008)
London_cent <- c(-0.118092, 51.509865)
Paris_cent  <- c(2.349014, 48.864716)

# Add the center_distance column
df <- df %>%
  mutate(
    center_distance = case_when(
      City == "Berlin" ~ distm(cbind(longitude, latitude), Berlin_cent, fun = distVincentySphere) / 1000,
      City == "London" ~ distm(cbind(longitude, latitude), London_cent, fun = distVincentySphere) / 1000,
      City == "Paris"  ~ distm(cbind(longitude, latitude), Paris_cent, fun = distVincentySphere) / 1000
    ),
    Business_Owned = ifelse(calculated_host_listings_count > 10, 1, 0)
  )

# Define the new order of columns
new_order <- c(
  "id",
  "Date",
  "City",
  "Currency",
  "Type",
  "Rating",
  "Bedroom",
  "Beds",
  "Bath",
  "Half_bath",
  "is_studio",
  "price",
  "Price_EUR",
  "neighbourhood_group",
  "neighbourhood",
  "latitude",
  "longitude",
  "center_distance",
  "room_type",
  "minimum_nights",
  "number_of_reviews",
  "last_review",
  "reviews_per_month",
  "calculated_host_listings_count",
  "availability_365",
  "number_of_reviews_ltm",
  "Business_Owned"
)

# Apply the new order of columns
df <- df[, new_order]

write.csv(df, "./data/df.csv", row.names = FALSE)

