######################################################################################################
# D03a. Introduction to ggplot2
# By William Yu, UCLA Anderson Forecast
# updated on 10/7/2018
##################################################################################################### 

# Grammar of Graphics
# Reference: https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

# setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Data")

install.packages("ggplot2") 
install.packages("tidyverse") 
library(ggplot2)
library(readxl) 
library(tidyverse)
library(dplyr)

land <- data.frame(read_excel("W03a_landvalue.xlsx"))  
names(land)

hist(land$homevalue)

# In ggplot land aesthetic (aes) means "something you can see". Examples include:
# position (i.e., on the x and y axes)
# color ("outside" color)
# fill ("inside" color)
# shape (of points)
# linetype
# size

# Geometric (geom) objects are the actual marks we put on a plot. Examples include:
# points (geom_point, for scatter plots, dot plots, etc)
# lines (geom_line, for time series, trend lines, etc)
# boxplot (geom_boxplot, for, well, boxplots!)

ggplot(land, aes(x = homevalue)) + geom_histogram()
ggplot(land, aes(x = homevalue)) + geom_histogram(stat = "bin", binwidth=4000)

ggplot(filter(land, state %in% c("CA", "TX")),
       aes(x=date, y=homevalue, color=state)) + geom_point()

help.search("geom_", package = "ggplot2")

hv2016q1 <- filter(land, date == "2016Q1") 
ggplot(hv2016q1,aes(y = structurecost, x = landvalue)) + geom_point()
ggplot(hv2016q1,aes(y = structurecost, x = log(landvalue))) + geom_point()

hv2016q1$pred.sc <- predict(lm(structurecost ~ log(landvalue), data = hv2016q1))
p1 <- ggplot(hv2016q1, aes(x = log(landvalue), y = structurecost))
p1
p1 + geom_point() + geom_line(aes(y = pred.sc), color="red")
p1 + geom_text(aes(label=state), size=3)
p1 + geom_point() + geom_line(aes(y = pred.sc))+ geom_text(aes(label=state), size = 3, nudge_x = .25)

install.packages("ggrepel") 
library("ggrepel")

p1 + geom_point() + geom_line(aes(y = pred.sc), color="red") + 
  geom_text_repel(aes(label=state), size = 3)

#
# Corrleation between Corruption Percention Index (CPI) and GDP per Capita
#

wdi <- data.frame(read_excel("W03b_wdi.xlsx"))     # World Development Indicators
cpi <- data.frame(read_excel("W03c_cpi.xlsx", skip=2))  
colnames(cpi)[4]="cpi" # rename Column #4 to cpi

wdiyp <- subset(wdi, Indicator.Code=="NY.GDP.PCAP.CD") # Extract data only for GDP per capita
wdiyp16 = wdiyp[,c("Country.Code", "X2016")]
colnames(wdiyp16)=c("ISO3","gdppc")

cpigdp=merge(cpi,wdiyp16, by="ISO3") # Merge two dataset by country code

ggplot(cpigdp, aes(x = cpi, y = gdppc)) + geom_point()
ggplot(cpigdp, aes(x = cpi, y = log(gdppc))) + geom_point()
p1=ggplot(cpigdp, aes(x = cpi, y = gdppc,color=Region))
p1+geom_point()

p2 <- p1 +
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "loess",
              formula = y ~ x , se = FALSE,
              color = "black")
p2 + geom_point()

# A look at all 25 symbols
df <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
s <- ggplot(df, aes(x = x, y = y))
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()

p2 + geom_point(shape = 1, size = 2)
p3=p2 + geom_point(shape = 1, size = 3, stroke=1.5)
p3
p3 + geom_text_repel(aes(label = ISO3), size=2)

country.label = c("Russia", "Iraq", "Argentina", "Sudan", "Afghanistan", "Congo", "Greece", "Brazil",
                  "India", "Italy", "China", "South Africa", "Botswana", "Bhutan", "Rwanda", "France",
                  "United States of America", "Germany", "Norway", "Japan", "New Zealand", "Singapore")

p4 = p3 + geom_text(aes(label = Country),
            color = "gray20", data = filter(cpigdp, Country %in% country.label))
p4

p5 = p4+
scale_x_continuous(name = "Corruption Perceptions Index, 2017 (100=least corrupt)")+
scale_y_continuous(name = "Log GDP Per Capita, 2016") +
ggtitle("Corruption and GDP Per Capita")
p5

#
# Correlation between Life Expectancy and Fertility Rate
#

# Get variables for life expetancy, total population, and fertility rate
wdi.le = subset(wdi, Indicator.Code == "SP.DYN.LE00.IN")
wdi.po = subset(wdi, Indicator.Code == "SP.POP.TOTL")
wdi.fr = subset(wdi, Indicator.Code == "SP.DYN.TFRT.IN")

wdi3v.16a=merge(wdi.le[,c("Country.Name","Country.Code","X2016")], wdi.po[,c("Country.Code","X2016")], by="Country.Code")
wdi3v.16b=merge(wdi3v.16a, wdi.fr[,c("Country.Code","X2016")], by="Country.Code")

# Remove those rows that are not countries but regions
wdi3v.16=wdi3v.16b[!wdi3v.16b$Country.Code %in% c("ARB",	"CSS",	"CEB",	"EAR",	"EAS",	"EAP",	"TEA",	"EMU",	"ECS",
                                                  "ECA",	"TEC",	"EUU",	"FCS",	"HPC",	"HIC",	"IBD",	"IBT",	"IDB",	
                                                  "IDX",	"IDA",	"LTE",	"LCN",	"LAC",	"TLA",	"LDC",	"LMY",	"LIC",	
                                                  "LMC",	"MEA",	"MNA",	"TMN",	"MIC",	"NAC",	"INX",	"OED",	"OSS",	
                                                  "PSS",	"PST",	"PRE",	"SST",	"SAS",	"TSA",  "SSF",	"SSA",	"TSS",
                                                  "UMC",	"WLD"),]

colnames(wdi3v.16)=c("code", "country", "life","pop","fr")
p6=ggplot(wdi3v.16, aes(x=life, y=fr, size=pop)) + geom_point(alpha=0.4) + scale_size_continuous(range=c(0.5, 20)) 
p6

