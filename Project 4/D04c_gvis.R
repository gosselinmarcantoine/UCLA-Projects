######################################################################################################
# D04a Dynamic Corrleation via GoogleVis
# By Joao de Paula, UCLA Extension Data Science Intensive 
# 10/13/2018
######################################################################################################

## Data accessed from the World Bank via their API and displayed with a Motion Chart.

install.packages("googleVis") # provides an interface between R and the Google Charts API
install.packages("RJSONIO") # Serialize R objects to JSON, JavaScript Object Notation
library(googleVis)


# Functions to connect to World Bank API to retrieve data (JSON format) 
# https://datahelpdesk.worldbank.org/knowledgebase/articles/898599-api-indicator-queries

#getWorldBankData Function

getWorldBankData <- function(id='SP.POP.TOTL', date='1960:2017',
                             value="value", per.page=14000){
  require(RJSONIO)
  url <- paste("http://api.worldbank.org/countries/all/indicators/", id,
               "?date=", date, "&format=json&per_page=", per.page,
               sep="")
  
  wbData <- fromJSON(url)[[2]]
  
  wbData = data.frame(
    year = as.numeric(sapply(wbData, "[[", "date")),
    value = as.numeric(sapply(wbData, function(x)
      ifelse(is.null(x[["value"]]),NA, x[["value"]]))),
    country.name = sapply(wbData, function(x) x[["country"]]['value']),
    country.id = sapply(wbData, function(x) x[["country"]]['id'])
  )
  
  names(wbData)[2] <- value
  
  return(wbData)
}

## getWorldBankCountries Function for Countries - Latitude - Longitude - Region

getWorldBankCountries <- function(){
  require(RJSONIO)
  wbCountries <-
    fromJSON("http://api.worldbank.org/countries?per_page=14000&format=json") 
  wbCountries <- data.frame(t(sapply(wbCountries[[2]], unlist)))
  wbCountries$longitude <- as.numeric(wbCountries$longitude)
  wbCountries$latitude <- as.numeric(wbCountries$latitude)
  levels(wbCountries$region.value) <- gsub(" \\(all income levels\\)",
                                           "", levels(wbCountries$region.value))
  return(wbCountries)
}


## Use Sys.Date() to create "years" range to use in queries
years <- paste("1960:", format(Sys.Date(), "%Y"), sep="") 


## Get Fertility rate Data
fertility.rate <- getWorldBankData(id='SP.DYN.TFRT.IN',
                                   date=years, value="fertility.rate")

## Get Life Expectancy Data 
life.exp <- getWorldBankData(id='SP.DYN.LE00.IN',  date=years,
                             value="life.expectancy") 

## Get Population Data
population <- getWorldBankData(id='SP.POP.TOTL',  date=years,
                               value="population")

## Get GDP per capita Data in US$
GDP.per.capita <- getWorldBankData(id='NY.GDP.PCAP.CD',
                                   date=years,
                                   value="GDP.per.capita.Current.USD") 

## Merge Datasets
wbData <- merge(life.exp, fertility.rate)
wbData <- merge(wbData, population)
wbData <- merge(wbData, GDP.per.capita)

head(wbData)
dim(wbData) #14000 observations 7 variables

## Get mappings for all countries and regions
wbCountries <- getWorldBankCountries()
dim(wbCountries)
head(wbCountries)

## Add regional information
wbData <- merge(wbData, wbCountries[c("iso2Code", "region.value", 
                                      "incomeLevel.value")],
                by.x="country.id", by.y="iso2Code")
names(wbData)

## Subset Dataset 
subData <- subset(wbData, !region.value %in% "Aggregates" , select=
                    -country.id) 

## Create Dynamic Visualization
J <- gvisMotionChart(subData, idvar="country.name", timevar="year",
                     options=list(width=700, height=600))

plot(J)

# It will open browser to show visulization, the site needs Flash 
# to work & # internet connection is required.
# 
# If a website isn't working, you might need to change your settings to allow Flash.
# 
# To enable Flash on Chrome:
# To the left of the web address, click (i) (next to url) or Info View site information.
# At the bottom, click Site Settings.
# In the new tab, to the right of "Flash," click the Down and then Allow. Close tab.
# Go back to the site and reload the page.