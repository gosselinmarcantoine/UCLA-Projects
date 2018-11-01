### UCLA Data Science Class (Project 1)

# Problem A ---------------------------------------------------------------

TaylorMortgage <- 
  function(Loan, InterestRate, LoanLength) {
    c <- InterestRate / 12
    n <- LoanLength * 12
    MonthlyPayment <- Loan * (c * (1 + c) ^ n) / (((1 + c) ^ n) - 1)
    return(round(MonthlyPayment, digits = 2))
  }

TaylorMortgage(465600, 0.045, 30) # Monthly payment of $2359.13


# Problem B ---------------------------------------------------------------

setwd("~/Documents/DataScience/UCLADatascience/Project 1")

## Load packages
library(tidyverse)
library(readxl)

## Load project 1 data
zipcode_data <- read_excel("P01_LA zipcode payroll.xlsx")

## Cleaning the data

# Replacing ***** (null values) with NA.
# Investigating the data reveals only Employement and wages have missing values.
zipcode_data$Employment <- ifelse(zipcode_data$Employment == "*****", 0, zipcode_data$Employment)
zipcode_data$Wages <- ifelse(zipcode_data$Wages == "*****", 0, zipcode_data$Wages)

# Converting column 5 and 6 to numeric class.
zipcode_data$Employment <- as.numeric(zipcode_data$Employment)
zipcode_data$Wages <- as.numeric(zipcode_data$Wages)

# Replacing "Professional, Scientific, & Technical Skills" with "Professional".
zipcode_data$Industry <- ifelse(zipcode_data$Industry == "Professional, Scientific, & Technical Skills", "Professional", zipcode_data$Industry)

# Restricting the rows to "Information", "Professional, Scientific, & Technical Skills", and Total industries.
zipcode_data <- zipcode_data %>% filter(Industry == "Information" | Industry == "Professional" | is.na(Industry))

# Fixing all the zipcodes into a string of 5 digits.
zipcode_data$`Zip Code` <- substr(zipcode_data$`Zip Code`, 1, 5)

# Trying the spread function to make the data tidy
zipcode_data1 <- zipcode_data %>% spread(key = Industry, value = Employment) %>% select(`Zip Code`, "<NA>", Information, Professional)

# Making data look nice
zipcode_data1 <- zipcode_data1 %>% select(`Zip Code`, "<NA>", Information, Professional)
names(zipcode_data1) <- c("zipcode", "total", "information", "professional")

# Combine rows for the same zipcode resulting from the spread function.
zipcode_data2 <- zipcode_data1 %>% group_by(zipcode) %>% summarise_all(sum, na.rm = TRUE)

## Adding Information

# Adding a percent_wage column.
zipcode_data2 <- zipcode_data2 %>% group_by(zipcode) %>% mutate(per = (information + professional) / total)

# If we wanted to make the same exact appearance as professor Yu, we would:
# zipcode_data2 <- as.numeric(zipcode_data2$zipcode) but I think this looks better and reveals more (zipcodes are not numbers but identifiers/ labels)

# Creating a CSV file. I think it looks better without quotes and row.names but it would be easy to add them back.
write.csv(x = zipcode_data2, file = "techjobsdemographic.csv")

View(zipcode_data2)
