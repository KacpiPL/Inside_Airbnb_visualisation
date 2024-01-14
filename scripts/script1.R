library(dplyr)
library(tidyr)
library(stringr)

rm(list=ls())

# Load all datasets
df_berlin <- read.csv("./data/Berlin_listings.csv")
df_london <- read.csv("./data/London_listings.csv")
df_paris <- read.csv("./data/Paris_listings.csv")

# Add City and curency columns to each dataset
df_berlin$City <- "Berlin"
df_berlin$Currency <- "EUR"

df_london$City <- "London"
df_london$Currency <- "GBP"

df_paris$City <- "Paris"
df_paris$Currency <- "EUR"

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
name <- df[1, "name"]
name

# Separate the data
df1 <- df %>%
  mutate(
    Type = str_extract(name, "^[^·]+"),
    Rating = str_extract(name, "\\★[^·]+"),
    Bedroom = str_extract(name, "(?i)(\\d+ bedrooms?|studio)"),        # sometimes instead of bedroom there is "Studio".  "\\d+ (bedroom(s)? | Studio)"
    Beds = str_extract(name, "\\d+ bed(s)?\\b"),
    Bath = str_extract(name, "\\d+\\s+(private|shared)?\\s*bath")
  )

# Check if regex works correctly
rating_null <- df1[is.na(df1$Rating), ]
bedroom_null <- df1[is.na(df1$Bedroom), ]
beds_null <- df1[is.na(df1$Beds), ]       # we have also "Studio" here - how to handle it?
bath_null <- df1[is.na(df1$Bath), ]       # Half-bath ???

df <- df1
# Remove unnecessary variables
rm(rating_null, bedroom_null, beds_null, bath_null, df1, name, unnecessary_columns)

# Clean columns Rating, Bedroom, Beds, Bath
df$Rating <- as.numeric(gsub("★", "", df$Rating))
df$Bedroom <- as.numeric(gsub( " .*$", "", df$Bedroom))
df$Beds <- as.numeric(gsub( " .*$", "", df$Beds))
df$Bath <- as.numeric(gsub( " .*$", "", df$Bath))

# Delete column "name"
df <- df[ , !names(df) %in% "name"]

# Add column Price_EUR
## GBP/EUR at the beginning of December = 1.16
df$Price_EUR <- ifelse(df$Currency == "EUR", df$price, df$price * 1.16)

write.csv(df, "./data/df.csv")










