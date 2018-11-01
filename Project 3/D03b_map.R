######################################################################################################
# D03b. Mapping
# By William Yu, UCLA Anderson Forecast
# updated on 10/7/2018
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Project/project2")

install.packages("ggplot2") 
library(ggplot2)
library(readxl) 
library(dplyr)

#
# US County Mapping
# Reference: https://mgimond.github.io/ES218/Week12a.html
#

install.packages("maps")
library(maps)

cnty = map_data("county")
gusa = map_data("state")
head(cnty)
head(county.fips)

cnty2 = cnty %>% mutate(polyname=paste(region,subregion, sep=",")) %>%
  left_join(county.fips, by="polyname")
head(cnty2)

chci <- read.csv("chci.csv")
colnames(chci)[2]="fips"   # change the column name of "id" to "fips"
cnty3 = cnty2 %>% left_join(chci, by="fips")
head(cnty3)

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci16), colour = rgb(1,1,1,0.2)) + coord_quickmap() 

# Without County boundry --> colour = NA
ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci16), colour = NA) + coord_quickmap() +
  scale_fill_gradient(low="lightyellow2", high="darkgreen")

install.packages("RColorBrewer")
library(RColorBrewer)

# Generating a divergent color scheme
qt1=quantile(cnty3$chci16, probs=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=T)
cnty3$chci16a =cut(cnty3$chci16, breaks=qt1, labels=paste(qt1[-1]))

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci16a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu")

# Other palettes-bidirection:Spectral,RdYlGn,RdYlBu,RdGy,PuOr,PRGn,PiYG, BrBG
# Other palettes-onedirection:YlOrRd,YlOrBr,YlGnBu,YlGn,Reds,RdPu,Purples,PuRd,
# PuBuGn,PuBu,OrRd,Oranges,Greys,Greens,GnBu,BuGn,Blues

state_layer=geom_polygon(aes(long,lat,group=group), fill=NA, data=gusa,color = "darkgreen") 

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci16a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu") + state_layer

#
# leaflet map
#

library(leaflet)
m =leaflet() %>%
   addTiles() %>% # add default openstreetmap map tiles
   addMarkers(lng=-118.4466131 , lat=34.0597587, 
              popup="Where we meet to learn Data Science!!!")
m

m2 =leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1=-118.456554 , lat1=34.078039,
    lng2=-118.436383 , lat2=34.062717, 
    fillColor = "transparent"
   )
m2

