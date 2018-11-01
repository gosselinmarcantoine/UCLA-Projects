# Project 3


## Part 1A

setwd("~/Documents/DataScience/UCLADatascience/Project 3")

# Install and load packages -----------------------------------------------

install.packages("RColorBrewer")
install.packages("ggplot2") 
install.packages("maps")
library(ggplot2)
library(readxl) 
library(dplyr)
library(RColorBrewer)
library(maps)

#
# US County Mapping
# Reference: https://mgimond.github.io/ES218/Week12a.html
#


# Formatting the US map and chci data -------------------------------------

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


# Plotting  ---------------------------------------------------------------

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci09), colour = rgb(1,1,1,0.2)) + coord_quickmap() 

# Without County boundry --> colour = NA
ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci09), colour = NA) + coord_quickmap() +
  scale_fill_gradient(low="lightyellow2", high="darkgreen")

# Generating a divergent color scheme
qt1=quantile(cnty3$chci16, probs=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=T)
cnty3$chci09a =cut(cnty3$chci09, breaks=qt1, labels=paste(qt1[-1]))

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci09a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu")

# Other palettes-bidirection:Spectral,RdYlGn,RdYlBu,RdGy,PuOr,PRGn,PiYG, BrBG
# Other palettes-onedirection:YlOrRd,YlOrBr,YlGnBu,YlGn,Reds,RdPu,Purples,PuRd,
# PuBuGn,PuBu,OrRd,Oranges,Greys,Greens,GnBu,BuGn,Blues

state_layer=geom_polygon(aes(long,lat,group=group), fill=NA, data=gusa,color = "darkgreen") 

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci09a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu") + state_layer


# Save as PDF -------------------------------------------------------------

ggsave("CHCIUS09.pdf", width=4, height=4)

## Part 1B

# load libraries ----------------------------------------------------------

library(readxl) 
library(tidyverse)
library(dplyr)

# Format data -------------------------------------------------------------

install.packages("ggrepel")
library(ggrepel)
library(readxl)

# Loading datas -----------------------------------------------------------

wdi <- data.frame(read_excel("W03b_wdi.xlsx"))     # World Development Indicators
cpi <- data.frame(read_excel("W03c_cpi.xlsx", skip=2))
colnames(cpi)[4]="cpi" # rename Column #4 to cpi


# Preparing data ----------------------------------------------------------

wdiyp60 = wdiyp[,c("Country.Code", "X1960")]
colnames(wdiyp60)=c("ISO3","gdppc")

cpigdp=merge(cpi,wdiyp60, by="ISO3") # Merge two dataset by country code

# Transforming data -------------------------------------------------------

# Get variables for life expetancy, total population, and fertility rate
wdi.le = subset(wdi, Indicator.Code == "SP.DYN.LE00.IN")
wdi.po = subset(wdi, Indicator.Code == "SP.POP.TOTL")
wdi.fr = subset(wdi, Indicator.Code == "SP.DYN.TFRT.IN")

wdi3v.60a=merge(wdi.le[,c("Country.Name","Country.Code","X1960")], wdi.po[,c("Country.Code","X1960")], by="Country.Code")
wdi3v.60b=merge(wdi3v.60a, wdi.fr[,c("Country.Code","X1960")], by="Country.Code")

# Remove those rows that are not countries but regions
wdi3v.60=wdi3v.60b[!wdi3v.60b$Country.Code %in% c("ARB",	"CSS",	"CEB",	"EAR",	"EAS",	"EAP",	"TEA",	"EMU",	"ECS",
                                                  "ECA",	"TEC",	"EUU",	"FCS",	"HPC",	"HIC",	"IBD",	"IBT",	"IDB",	
                                                  "IDX",	"IDA",	"LTE",	"LCN",	"LAC",	"TLA",	"LDC",	"LMY",	"LIC",	
                                                  "LMC",	"MEA",	"MNA",	"TMN",	"MIC",	"NAC",	"INX",	"OED",	"OSS",	
                                                  "PSS",	"PST",	"PRE",	"SST",	"SAS",	"TSA",  "SSF",	"SSA",	"TSS",
                                                  "UMC",	"WLD"),]

colnames(wdi3v.60)=c("code", "country", "life","pop","fr")

# Drawing plot ------------------------------------------------------------

p6=ggplot(wdi3v.60, aes(x=life, y=fr, size=pop)) + geom_point(alpha=0.4) + scale_size_continuous(range=c(0.5, 20)) 
p6


## Part B

# Load zillow data --------------------------------------------------------

# P03
# Zillow Data Exploratory Analysis
# Adapted from https://www.kaggle.com/c/zillow-prize-1/kernels

setwd("~/Documents/DataScience/UCLADatascience/Project 3")

install.packages("DT")  
install.packages("corrplot")  
install.packages("leaflet")  
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

setwd("~/Documents/DataScience/UCLADatascience/Project 3/zillow prize project data")
properties <- read.csv('properties_2016.csv')
transactions <- read.csv('train_2016_v2.csv')
sample_submission <- read.csv('sample_submission.csv')

setwd("~/Documents/DataScience/UCLADatascience/Project 3")

names(properties)
names(transactions)
head(transactions)
names(sample_submission)
head(sample_submission)

table(properties$yearbuilt)
hist(properties$yearbuilt)
table(transactions$transactiondate)

# Rename the variable names
# FunctionX(dataA) is the same as dataA %>% functionX

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

# Convert dummary variables (Y and N) to (1 and 0)
properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

# Take a look at the data
properties <- properties %>% select(id_parcel, build_year, starts_with("area_"), 
                                    starts_with("num_"), starts_with("flag_"), starts_with("region_"), everything())
datatable(head(properties,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
datatable(head(transactions,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))

# Take a look at the transaction data
tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))
tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-03-20"))),size=1)

# Distribution of Zestaimate's forecast errors (log rror)
# logerror: log(Zestimate) - log(Saleprice). So a positive logerror means Zestimate is overestimating the Saleprice, 
# a negative logerror means that Zestimate is underestimating Saleprice. 
# absolute logerror: a small value means that log(Zestimate) is close to log(Saleprice). 
# So, Zestimate predictions are close to Saleprice.

transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))

# Absolute logerror
transactions <- transactions %>% mutate(abs_logerror = abs(logerror))
# it is the same as: transactions$abs_logerror=abs(transactions$logerror)

transactions %>% 
  ggplot(aes(x=abs_logerror)) + 
  geom_histogram(bins=400, fill="orange")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,0.5))

# How does log error change with time
transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_logerror)) + 
  geom_line(size=1.5, color="red")+geom_point(size=5, color="red")+theme_bw()

# Missing values management
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

good_features <- filter(missing_values, missing_pct<0.75)

# Correlation with absolute logerror
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
corrplot(cor(tmp, use="complete.obs"),type="lower")

vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
corrplot(cor(tmp, use="complete.obs"), type="lower")

vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
corrplot(cor(tmp, use="complete.obs"), type="lower")

# Correlation with logerror
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"logerror")))
corrplot(cor(tmp, use="complete.obs"),type="lower") # got some correlation

vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))
corrplot(cor(tmp, use="complete.obs"), type="lower") # No correlation

vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))
tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))
corrplot(cor(tmp, use="complete.obs"), type="lower") # no correlation

# Distribution of the year house built 
cor_tmp %>% 
  ggplot(aes(x=build_year))+geom_line(stat="density", color="red", size=1.2)+theme_bw()

# How does the absolute logerror change with build_year?
cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_abs_logerror = mean(abs(logerror)),n()) %>% 
  ggplot(aes(x=build_year,y=mean_abs_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.25))+theme_bw()

# How does the logerror change with build_year?
cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=build_year,y=mean_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.075))+theme_bw()

# build_year has a complex relationship with log error. More variability 
# and avg logerror from early build_years. I wonder what it looks like when
# plotting the same graph with sales price associated to obs. color.

transactions <- transactions %>% mutate(percentile = cut(abs_logerror,quantile(abs_logerror, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1),names = FALSE),include.lowest = TRUE,labels=FALSE))

tmp1 <- transactions %>% 
  filter(percentile == 1) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp2 <- transactions %>% 
  filter(percentile == 5) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp3 <- transactions %>% 
  filter(percentile == 3) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")

tmp1 <- tmp1 %>% mutate(type="best_fit")
tmp2 <- tmp2 %>% mutate(type="worst_fit")
tmp3 <- tmp3 %>% mutate(type="typical_fit")

tmp <- bind_rows(tmp1,tmp2,tmp3)
tmp <- tmp %>% mutate(type = factor(type,levels = c("worst_fit", "typical_fit", "best_fit")))

col_pal <- "Set1"
tmp %>% ggplot(aes(x=latitude, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

tmptrans <- transactions %>% left_join(properties, by="id_parcel")
tmptrans %>% ggplot(aes(x=latitude,y=abs_logerror))+geom_smooth(color="red")+theme_bw()


tmp %>% ggplot(aes(x=longitude, fill=type, color=type)) + geom_line(stat="density", size=1.2) +
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

tmptrans %>% ggplot(aes(x=longitude,y=abs_logerror))+geom_smooth(color="red")+theme_bw()

tmp %>% ggplot(aes(x=area_total_finished, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(700,7.5e3))

tmptrans %>% ggplot(aes(x=area_total_finished,y=abs_logerror))+geom_smooth(color="red")+
  theme_bw()+ coord_cartesian(xlim=c(600,7.5e3),ylim=c(0.1,0.2))

tmp %>% ggplot(aes(x=area_live_finished, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,1e4))

tmp %>% ggplot(aes(x=num_room, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,10))

tmp %>% ggplot(aes(x=num_unit, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(1,4))

tmp %>% ggplot(aes(x=build_year, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)
# Better fit from recent build_year as mentionned earlier.

tmp %>% ggplot(aes(x=tax_total, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,1e6))

tmptrans %>% ggplot(aes(x=tax_total,y=abs_logerror))+geom_smooth(color="red")+theme_bw()+ 
  coord_cartesian(xlim=c(0,1e6),ylim=c(0.05,0.2))
# Cheaper houses tend to have a harder value to estimate.

tmp %>% ggplot(aes(x=tax_building, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
  theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,1e6))

# Where does Zestimate over or underpredict?
tmptrans <- tmptrans %>% mutate(overunder = ifelse(logerror<0,"under","over"))

tmptrans %>% ggplot(aes(x=latitude,y=abs(logerror), color=overunder))+geom_smooth()+
  theme_bw()+scale_color_brewer(palette="Set1")

tmptrans %>% ggplot(aes(x=longitude,y=abs(logerror), color=overunder))+geom_smooth()+
  theme_bw()+scale_color_brewer(palette="Set1")

# Both for latitude and longitude there is a range where Zestimate both under- and overpredicts. 
# Where is that?

leaflet() %>% 
  addTiles() %>% 
  fitBounds(-118.5,33.8,-118.25,34.15) %>% 
  addRectangles(-118.5,33.8,-118.25,34.15) %>% 
  addMiniMap()

# For properties with small calculated total area, Zestimate seems to overpredict.
tmptrans %>% filter(area_total_calc <=5000) %>% ggplot(aes(x=area_total_calc,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")

# Whereas for actual finished area there is no such effect. 
tmptrans %>% ggplot(aes(x=area_total_finished,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw()+coord_cartesian(xlim=c(500,7.5e+03),ylim=c(0,0.3))+scale_color_brewer(palette="Set1")

tmptrans %>% ggplot(aes(x=area_total_finished,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw()+coord_cartesian(xlim=c(500,7.5e+03),ylim=c(0,0.3))+scale_color_brewer(palette="Set1")

tmptrans %>% filter(area_lot <=1e+05) %>% ggplot(aes(x=area_lot,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")

tmptrans %>% ggplot(aes(x=num_room,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")

tmptrans %>% ggplot(aes(x=build_year,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")

tmptrans %>% ggplot(aes(x=tax_total,y=abs(logerror), color=overunder))+
  geom_smooth()+theme_bw() + coord_cartesian(xlim=c(0,1e+06),ylim=c(0,0.2))+scale_color_brewer(palette="Set1")

# Where are all those properties?
# Show 2,000 of the properties on the map.
lat <- range(properties$latitude/1e06,na.rm=T)
lon <- range(properties$longitude/1e06,na.rm=T)

tmp <- properties %>% 
  sample_n(2000) %>% 
  select(id_parcel,longitude,latitude) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon) %>% 
  left_join(transactions,by="id_parcel")

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE) %>% 
  addMiniMap()

# Map absolute logerror
# Show the absolute logerror on map. Red = higher.

tmp <- transactions %>% 
  sample_n(2000) %>% 
  left_join(properties,by="id_parcel") %>% 
  select(id_parcel,longitude,latitude, abs_logerror) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon, abs_logerror)

qpal <- colorQuantile("YlOrRd", tmp$abs_logerror, n = 7)

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE, color=~qpal(abs_logerror),fillOpacity = 1) %>% 
  addLegend("bottomright", pal = qpal, values = ~abs_logerror,title = "Absolute logerror",opacity = 1) %>% 
  addMiniMap()

# Census Track
# Looks like a number. However a closer look reveals its actually a number composed of two parts, 
# separated by a dot.

str(properties$rawcensustractandblock[1])
as.character(properties$rawcensustractandblock[1])

# 60378002.041
# FIPS Code (6037) - Tract Number (8002.04) - And block Number (1)

properties <- properties %>% mutate(census = as.character(rawcensustractandblock), 
                                    tract_number = str_sub(census,5,11), tract_block = str_sub(census,12))

# FIPS codes can be looked up (https://www.ffiec.gov/census/Default.aspx)
# 6037 Los Angeles
# 6059 Orange County
# 6111 Ventura County
# For example "6037 8002.04" lets look up some information:
# https://www.ffiec.gov/census/report.aspx?year=2016&county=037&tract=8002.04&state=06&report=demographic


# Compare & Contrast abs_logerror | logerror regressions ------------------

Fit1 = lm (abs_logerror ~ tax_property + area_live_finished + num_garage  + num_bedroom + num_bathroom, data=cor_tmp)
summary(Fit1)

Fit2 = lm (logerror ~ tax_property + area_live_finished + num_garage  + num_bedroom + num_bathroom, data=cor_tmp)
summary(Fit2)

# The difference between these simple linear regressions make sense; the absolute
  # log of the error means that errors above or below the actual sales price
  # compound while the log of the error has the errors above and below the actual
  # sales price counteract each other and results in statistical insignificance.

# Examining the absolute log of the errors (since the log error doesn't really
  # tell much) shows that the error (abs. log error) is associated with properties
  # that have a higher taxes, larger (area_live_finish), fewer bedrooms, and fewer
  # bathrooms. The number of garages, on the other hand, does not provide any 
  # intel on whether the zestimate will produce over or under estimations.