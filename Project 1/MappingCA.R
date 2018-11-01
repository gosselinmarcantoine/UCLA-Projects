
setwd("~/Documents/DataScience/UCLADatascience/Project 1")

## Load packages
library(tidyverse)
library(readxl)
library(dplyr)

## Load project 1 data
zipcode <- read_excel("P01_LA zipcode payroll.xlsx")

# Data Pre-processing -----------------------------------------------------

# Replacing ***** (null values) with NA.
# Investigating the data reveals only Employement and wages have missing values.
zipcode$Employment <- ifelse(zipcode$Employment == "*****", 0, zipcode$Employment)
zipcode$Wages <- ifelse(zipcode$Wages == "*****", 0, zipcode$Wages)

# Converting columns to numeric class.
zipcode$Employment <- as.numeric(zipcode$Employment)
zipcode$Wages <- as.numeric(zipcode$Wages)
zipcode$`Zip Code` <- as.factor(zipcode$`Zip Code`)

# Shortening Industry Names
zipcode$Industry <- ifelse(zipcode$Industry == "Professional, Scientific, & Technical Skills", "Professional", zipcode$Industry)

# Restricting the rows to "Information", "Professional, Scientific, & Technical Skills", and Total industries.
zipcode <- zipcode %>% filter(Industry == "Information" | Industry == "Professional" | is.na(Industry))

# Fixing all the zipcodes into strings of 5 digits.
zipcode$`Zip Code` <- substr(zipcode$`Zip Code`, 1, 5)

# Trying the spread function to make the data tidy
zipcode <- zipcode %>% spread(key = Industry, value = Employment) %>% select(`Zip Code`, "<NA>", Information, Professional)

# Making data look nice
zipcode <- zipcode %>% dplyr::select(`Zip Code`, "<NA>", Information, Professional)
names(zipcode) <- c("zip", "total", "information", "professional")

# Combine rows for the same zipcode resulting from the spread function.
zipcode <- zipcode %>% group_by(zip) %>% summarise_all(sum, na.rm = TRUE)

# Adding a percent_wage column
zipcode <- zipcode %>% group_by(zip) %>% mutate(per = (information + professional) / total)

# Visualizing -------------------------------------------------------------

library(sf)
library(ggplot2)
library(tigris)
library(leaflet)
library(sp)

# Adding longitude and latitude for popups/markers
popups <- read.csv("https://gist.githubusercontent.com/erichurst/7882666/raw/5bdc46db47d9515269ab12ed6fb2850377fd869e/US%2520Zip%2520Codes%2520from%25202013%2520Government%2520Data", header = TRUE)

popups$ZIP <- as.character(popups$ZIP)

## Adding zipcode bounderies file from package tigris 
zip<-zctas(cb=TRUE,class="sf") # sf type is simple features: spatial 

# Combining popups, zip, and zipcode
zip.sf <- zipcode %>% inner_join(popups, by = c("zip" = "ZIP")) %>% inner_join(zip, by = c("zip" = "GEOID10"))

leaflet()%>%
  addTiles()%>%
  addMarkers(data = zip.sf, lng = ~LNG, lat = ~LAT) #%>% 
  addPolygons(fillColor = zip.sf$professional, opacity = .5)
