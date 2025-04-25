# Load libraries
library(dplyr)
library(lubridate)
library(GSODR)
# list of Dutch cities 
cities <- data.frame(
  city = c("Amsterdam", "Rotterdam", "The Hague", "Utrecht", "Eindhoven", "Tilburg", "Groningen", "Almere", "Breda", "Nijmegen", "Enschede", "Haarlem", "Arnhem", "Zaanstad", "Maastricht", "Apeldoorn"),
  lat = c(52.37, 51.92, 52.08, 52.09, 51.44, 51.56, 53.22, 52.37, 51.59, 51.84, 52.22, 52.39, 51.98, 52.45, 50.85, 52.21),
  lon = c(4.89, 4.48, 4.30, 5.11, 5.48, 5.09, 6.57, 5.22, 4.78, 5.85, 6.89, 4.64, 5.92, 4.81, 5.69, 5.97)
)

# Find closest weather stations
cities$stations <- sapply(1:NROW(cities), function(j){
    nearest_stations(cities$lat[j], cities$lon[j], 100) %>% 
    filter(END >= 20240000) %>% 
    .[which.min(distance_km), ] %>%
    pull(STNID)
})
weather_data <- get_GSOD(years = c(2000:2024), station = cities$stations)
saveRDS(weather_data, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate.rds")
data <- readRDS("/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate.rds")
head(data)
weather_data <- data

library(dplyr)
weather_data  %>% group_by(NAME) %>% summarise(mean(TEMP))


plot(weather_data)






saveRDS(weather_data, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate.rds")
write.csv(weather_data, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate.csv")
haven::write_dta(weather_data, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate.dta")
openxlsx::write.xlsx(
    weather_data, 
    "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate.xlsx"
)



temp_wide <- reshape(weather_data[, c("YEARMODA", "NAME", "TEMP")],
                     timevar = "NAME",
                     idvar = "YEARMODA",
                     direction = "wide")

# Remove rows with any missing values
temp_wide <- temp_wide[complete.cases(temp_wide), ]

# Drop the date column and prepare data for plotting
#temp_matrix <- temp_wide[, -1]

# Optional: Rename columns to remove the "TEMP." prefix
colnames(temp_wide) <- c("date", sub("TEMP\\.", "", colnames(temp_matrix)))

# Create the pairs plot
quartz()
plot(temp_wide[, -1])
saveRDS(temp_wide, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/temparature.RDS")



# Step 1: Load and prepare the data
temp_debilt <- temp_wide[, c("date", "DE BILT")] %>% 
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-12-31"))
temp_sorted <- temp_debilt[order(temp_debilt[, 2]), ]

# Step 2: Half-way search function
halfway_search <- function(temp_vector, threshold) {
  low <- 1
  high <- nrow(temp_vector)
  steps <- 0
  result_index <- NA
  
  while (low <= high) {
    steps <- steps + 1
    mid <- floor((low + high) / 2)
    mid_val <- temp_vector[mid, 2]
    
    if (mid_val < threshold) {
      low <- mid + 1
    } else {
      result_index <- mid
      high <- mid - 1
    }
  }
  
  if (!is.na(result_index)) {
    cat("Temperature closest to", threshold, "°C is", temp_vector[result_index, 2], 
        "at ", as.character(temp_vector[result_index, 1]), "\n")
    cat("Steps taken:", steps, "\n")
  } else {
    cat("No temperature found.\n")
  }
}

# Step 3: Test the function
halfway_search(temp_sorted, 20)
