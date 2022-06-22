# 1. Info
#Author: Robin Pfaff
#ZHAW Datachallenge Juni 2022

# 2. dokument einrichten

setwd("~/Library/CloudStorage/OneDrive-ZHAW/4. Semester/Data Challenge/datachallenge_git/datachallenge_deer")

# bibliotheken laden
library(tidyverse)
library(sf)
library(lubridate)


# 3. datenimport
deer_raw <- read_csv("data_hidden/all_deer_cleaned.csv")
#geodaten von Waldareal bezogen über wfs kt ZH und ueber ArcGis als shapefile exportiert
sihlwald <- read_sf("data/Waldareal_wgs84.shp")

# 4. rohdaten koordinatensystem zuweisen
sihlwald_21781 <- st_transform(sihlwald, 21781)


times <- seq(2, 23, 3)

get_closest_multiple <- function(input, sequence){
  sapply(input, function(x){sequence[which.min(abs(x - sequence))]})
}

deer_raw %>%
  mutate(
    time_dec = hour(datetime_utc) + minute(datetime_utc)/60,
    time_close = get_closest_multiple(time_dec, times),
    time_diff = abs(time_dec-time_close),
    timelag = as.integer(difftime(lead(datetime_utc), datetime_utc, units = "secs"))
         ) %>%
filter(time_diff < 0.05) -> deer

# 5. funktion erstellen mit Grafik als output

reh_grafics <- function(reh_number, year, month){
  
  # ein Tier herausfiltern
  reh_number_output <- filter(deer, reh == reh_number)
  
  # einen Monat filtern
  reh_number_month_output <- reh_number_output %>%
    mutate(year1 = year(datetime_utc),
           month1 = month(datetime_utc)) %>%
    filter(year1 == year, month1 == month)
  
  # extent begranzen
  reh_number_month_output <- st_as_sf(reh_number_month_output, coords = c("y", "x"), crs = 21781)
  reh_bbox <- st_bbox(reh_number_month_output) %>% st_as_sfc() %>% st_set_crs(21781) %>% st_buffer(200)
  reh_bbox <- st_bbox(reh_bbox) %>% st_as_sfc() %>% st_set_crs(21781)
  
  # Herausfinden wie viele Datenpunkte innerhalb Waldfläche und wie viele ausserhalb
   st_agr(sihlwald_21781) = "constant" #this suppresses warning: attribute variables are assumed to be spatially constant throughout all geometries
   intersected_points <- st_intersection(x = sihlwald_21781, y = reh_number_month_output$geometry)
  
  # darstellen karte
  map <- ggplot() + 
    geom_sf(data = st_intersection(sihlwald_21781, reh_bbox), alpha = 0.2, fill = "green") +
    geom_sf(data = reh_number_month_output, color = "blue", alpha = 0.4) + 
    geom_sf(data = intersected_points, color = "red")
  
  #vorbereitung daten darstellen
  gesamt <- length(reh_number_month_output$geometry)
  innerhalb_wald <- length(intersected_points$geometry)
  ausserhalb_wald <- gesamt - innerhalb_wald
  aufenthaltsorte <- c("innerhalb wald", "ausserhalb wald")
  anzahl_in_out <- c(innerhalb_wald, ausserhalb_wald)
  df_aufenthalt <- data.frame(aufenthaltsorte, anzahl_in_out)

  #daten darstellen diagramm
  piechart <- ggplot(df_aufenthalt, aes(x="", y=anzahl_in_out, fill=aufenthaltsorte)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) + 
    theme_void() +
    labs(title = paste(year, month, sep = " "))
 
  # output definieren
   out <- list(map, piechart)
  return(out)
}

# funktion testen
reh_grafics("RE02", 2013, 10)


# 6. funktion erstellen mit vektor als output

reh_vector <- function(reh_number, year, month){
  
  # ein Tier herausfiltern
  reh_number_output <- filter(deer, reh == reh_number)
  
  # einen Monat filtern
  reh_number_month_output <- reh_number_output %>%
    mutate(year1 = year(datetime_utc),
           month1 = month(datetime_utc)) %>%
    filter(year1 == year, month1 == month)
  
  # extent begranzen
  reh_number_month_output <- st_as_sf(reh_number_month_output, coords = c("y", "x"), crs = 21781)
  
  # Herausfinden wie viele Datenpunkte innerhalb Waldfläche und wie viele ausserhalb
  st_agr(sihlwald_21781) = "constant" #this suppresses warning: attribute variables are assumed to be spatially constant throughout all geometries
  intersected_points <- st_intersection(x = sihlwald_21781, y = reh_number_month_output$geometry)
  
  #vorbereitung daten darstellen
  gesamt <- length(reh_number_month_output$geometry)
  innerhalb_wald <- length(intersected_points$geometry)
  ausserhalb_wald <- gesamt - innerhalb_wald
  aufenthalt <- data.frame(reh = reh_number, year = year, month = month, innerhalb = innerhalb_wald, ausserhalb = ausserhalb_wald)
  
  # output definieren
  return(aufenthalt)
}

#test function
reh_vector("RE01", 2013, 11)


 ### daten aufbereiten für input ###

input_data <- deer %>%
  mutate(year = year(datetime_utc),
         month = month(datetime_utc)) %>%
  filter(year == year, month == month) %>%
  transmute(reh = reh, year = year, month = month) %>%
  distinct(reh, year, month)

#for(i in 1:length(input_data$reh)){
y <- NULL
for(i in 1:length(input_data$reh)){
  result <- reh_vector(input_data$reh[i], input_data$year[i], input_data$month[i])
  y <- rbind(y, result)
}


map(1:2, function(i){
  reh_vector(input_data$reh[i], input_data$year[i], input_data$month[i])
})


data_in_out <- pmap_dfr(input_data, function(reh, year, month){
  reh_vector(reh, year, month)
})


avg_month <- data_in_out %>%
  group_by(month) %>%
  summarise(avg_innerhalb = mean(innerhalb), avg_ausserhalb = mean(ausserhalb))

avg_deer <- data_in_out %>%
  group_by(reh) %>%
  summarise(avg_innerhalb = mean(innerhalb), avg_ausserhalb = mean(ausserhalb))

reh_grafics("RE08", 2015, 03)
