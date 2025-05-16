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




cities <- data.frame(
  city = c("Leuven"),
  lat = c(50.88),
  lon = c(4.70)
)

# Find closest weather stations
cities$stations <- sapply(1:NROW(cities), function(j){
    nearest_stations(cities$lat[j], cities$lon[j], 100) %>% 
    filter(END >= 20240000) %>% 
    .[which.min(distance_km), ] %>%
    pull(STNID)
})
weather_data_leuven <- get_GSOD(years = c(2000:2024), station = cities$stations)
saveRDS(weather_data_leuven, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate_leuven.rds")
write.csv(weather_data_leuven, "/Users/tobias/Documents/professional/teaching/PMR/PIR_quardo/data/climate_leuven.csv")

