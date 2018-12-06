##################################################################################################
# P06
# Script Answer to Project 6, UCLA Extension Data Science Intensive
# 11/9/2018 by William Yu
##################################################################################################)

library(readxl) 
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(stringr)
library(DT)
library(corrplot)
library(lubridate)
setwd('~/Documents/DataScience/UCLADatascience/Project 7')
wdi <- read_excel("W03b_wdi.xlsx")     # World Development Indicators

remove1=c("X1960","X1961","X1962","X1963","X1964","X1965","X1966","X1967","X1968","X1969",
          "X1970","X1971","X1972","X1973","X1974","X1975","X1976","X1977","X1978","X1979","X1980")

# Remove years from 1960 to 1980
wdi=wdi[,!names(wdi) %in% remove1]

# Region country code
remove2=c("ARB","CEB","CSS","EAP","EAR","EAS","ECA","ECS","EUU","HIC","HPC","IBD","IBT","IDA","IDX",
          "LAC","LCN","LDC","LIC","LMC","LMY","MEA","MIC","MNA","NAC","OED","OSS","PRE","PSS","PST",
          "SAS","SSA","SSF","SST","TEA","TEC","TLA","TMN","TSA","UMC","WLD")

# Remove those regions, non-country data rows
wdi = wdi %>% filter(!`Country Code` %in% remove2)

# Subset data for dependent variable poverty gap in $3.2 
poverty = wdi %>% filter(`Indicator Code` %in% "SI.POV.LMIC.GP")

# Select those countries that have poverty data in 2012
pov12 = poverty %>% filter(!`2012` %in% NA)

pov12.list = pov12$`Country Code` %>% table() %>% names()

# Subset wdi with those countries with poverty data in 2012
wdi.pov = wdi %>% filter(`Country Code` %in% pov12.list)

# Transport the data frame between years and variables
wdi.povt=dcast(melt(as.data.table(wdi.pov), id.vars = c("Country Name", 
                "Country Code","Indicator Name", "Indicator Code")), 
      `Country Name` + `Country Code` + variable ~ `Indicator Code`, value.var = "value")

# Calculating missing values percentage
missing <- wdi.povt %>% summarize_all(funs(sum(is.na(.))/n()))
missing <- gather(missing, key="feature", value="missing_pct")
goodv <- filter(missing, missing_pct<0.25)

