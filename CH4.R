# Load required packages
library(data.table)

# Read CSV file into a data frame
data_frame <- read.csv("C:/Users/91846/Downloads/lvst_gases_MgC_2011.csv")

# Convert data frame into a data table
data_table <- as.data.table(data_frame)
names(data_table)
data_table<- data_table[, c("longitude", "latitude", "totCH4_MgC")]

# Install and load the ggmap package
install.packages("ggmap")
library(ggmap)

# Example latitude and longitude values

# Assuming 'data_frame' is your data frame and 'column_name' is the name of the column you want to convert to numeric
data_table$longitude <- as.numeric(data_table$longitude)
data_table$latitude <- as.numeric(data_table$latitude)
data_table<-data_table[totCH4_MgC!=0]
nrow(data_table)

install.packages("maps")
library(maps)

data_table$latitude <- as.numeric(data_table$latitude)
data_table$longitude <- as.numeric(data_table$longitude)

full_location <- map.where("world", x = data_table$longitude, y = data_table$latitude)

# Extract the country code from the full location string
extract_country_code <- function(location) {
  if (is.na(location)) {
    return(NA)
  } else {
    return(strsplit(location, ":")[[1]][1])
  }
}

# Apply the function to extract country codes
country_codes <- sapply(full_location, extract_country_code)

# Assign the country codes back to the data table
data_table$countrycode <- country_codes

# View the result
head(data_table)

table(data_table$countrycode)
data_table <- data_table[, c( "totCH4_MgC", "countrycode")]
names(data_table)
data_table <- aggregate(totCH4_MgC ~ countrycode, data = data_table, sum, na.rm = TRUE)
head(data_table)


install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")

library(ggplot2)
library(maps)
library(mapdata)

world_map <- map_data("world")
merged_data <- merge(world_map, data_table, by.x = "region", by.y = "countrycode", all.x = TRUE)
# Check column names of world_map
colnames(world_map)

# Check column names of data_table
colnames(data_table)
merged_data <- rbind(merged_data, merged_data[1, ])

library(dplyr)

# Group by country and calculate mean latitude and longitude
aggregated_data <- merged_data %>%
  group_by(region) %>%
  summarize(mean_long = mean(long),
            mean_lat = mean(lat),
            totCH4_MgC = first(totCH4_MgC))  # Take the first value of emissions (assuming it's constant for each country)

# Plot the map using the aggregated data
ggplot() +
  geom_map(data = aggregated_data, map = world_map,
           aes(x = mean_long, y = mean_lat, map_id = region, fill = totCH4_MgC),
           color = "black", size = 0.25) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "Total CH4 Emissions") +
  coord_fixed(1.3) +
  labs(title = "World CH4 Emissions Map - 2011") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))  # Centering the title


ggplot() +
  geom_map(data = merged_data, map = merged_data,
           aes(x = mean_long, y = mean_lat, map_id = region, fill = totCH4_MgC)) +
  scale_fill_gradient(low = "orange", high = "red", name = "Emissions") +
  coord_fixed(1.3) + # adjust this number to change the aspect ratio of the map
  labs(title = "World Emissions Map") +
  theme_void()

merged_data<-merged_data[, c( "region"  ,   "long"    ,   "lat"    ,  "totCH4_MgC")]
head(data_table)
