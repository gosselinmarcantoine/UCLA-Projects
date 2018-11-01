# P03
# Zillow Data Exploratory Analysis
# Adapted from https://www.kaggle.com/c/zillow-prize-1/kernels

# Project Setup -----------------------------------------------------------

setwd("~/Documents/DataScience/UCLADatascience/Project 4")

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

setwd("~/Documents/DataScience/UCLADatascience/Project 4")

# Data Exploration --------------------------------------------------------

names(properties)
names(transactions)
head(transactions)
names(sample_submission)
head(sample_submission)

table(properties$yearbuilt)
hist(properties$yearbuilt)
table(transactions$transactiondate)

# Data Preparation --------------------------------------------------------

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

# Data Exploration #2 -------------------------------------------------

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

# Data Preparation #2 -------------------------------------------------

# Missing values management
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

good_features <- filter(missing_values, missing_pct<0.25)

# Data Exploration #3 -----------------------------------------------------

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



# Data Pre-processing -----------------------------------------------------

subset_cor_tmp <- subset(cor_tmp, select = c("logerror", good_features[,1]))

subset_cor_tmp <- subset(subset_cor_tmp, select =c(-id_parcel, -fips, -latitude, -longitude, -zoning_landuse_county, -zoning_property, -rawcensustractandblock, -region_city, -region_zip, -censustractandblock, -tax_year, -tax_building, -tax_land, -flag_tub, -flag_fireplace))

corrplot(cor(subset_cor_tmp, use="complete.obs"),type="lower")

# High correlations: num_bathroom_calc, num_bathroom, num_bath; area_live_finsihed, area_total_calc; tax_total, tax_property. 
logerror <- subset_cor_tmp$logerror
subset_cor_tmp <- subset(subset_cor_tmp, select = c(-num_bathroom_calc, -num_bath, -area_total_calc, -tax_total))

str(subset_cor_tmp)

# We have three variables as int. While this seems to fit, the region_county and zoning_landuse varaibles are indices, reference numbers, or, in other words, labels. For that reason, we change the type to factor.
subset_cor_tmp$region_county <- as.factor(subset_cor_tmp$region_county)
subset_cor_tmp$zoning_landuse <- as.factor(subset_cor_tmp$zoning_landuse)
str(subset_cor_tmp)

# Model -------------------------------------------------------------------

mod <- lm(logerror ~ ., subset_cor_tmp)
summary(mod)

install.packages("leaps")
library(leaps)

fwd <- regsubsets(logerror ~ ., subset_cor_tmp, nvmax = 12, method = "forward")
summary(fwd)
bwd <- regsubsets(logerror ~ ., subset_cor_tmp, nvmax = 12, method = "backward")
summary(bwd)
both <- regsubsets(logerror ~ ., subset_cor_tmp, nvmax = 12)
summary(both)
names(summary(both)) # names of variable evaluation parameters

# I want the least amount of variables in our model, so I will use BIC

which.min(summary(fwd)$bic)
plot(summary(fwd)$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(5,summary(fwd)$bic[5],col="red",cex=2,pch=20)
which.min(summary(both)$bic)
which.min(summary(bwd)$bic)

coef(both,5)
coef(fwd, 5)
coef(bwd, 5)

# Model with Abs_logerror -------------------------------------------------

subset_cor_tmp$abs_logerror <- abs(subset_cor_tmp$logerror)

mod2 <- lm(abs_logerror ~ ., subset_cor_tmp)
summary(mod2)

fwd2 <- regsubsets(abs_logerror ~ ., subset_cor_tmp, nvmax = 12, method = "forward")
summary(fwd2)
bwd2 <- regsubsets(abs_logerror ~ ., subset_cor_tmp, nvmax = 12, method = "backward")
summary(bwd2)
both2 <- regsubsets(abs_logerror ~ ., subset_cor_tmp, nvmax = 12)
summary(both2)
names(summary(both2)) # names of variable evaluation parameters

# I want the least amount of variables in our model, so I will use BIC

which.min(summary(fwd2)$bic)
plot(summary(fwd2)$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(8,summary(fwd2)$bic[8],col="red",cex=2,pch=20)
which.min(summary(both2)$bic)
plot(summary(both2)$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(8,summary(both2)$bic[8],col="red",cex=2,pch=20)
which.min(summary(bwd2)$bic)
plot(summary(bwd2)$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(11,summary(bwd2)$bic[11],col="red",cex=2,pch=20)

coef(both2,8)
coef(fwd2, 8)
coef(bwd2, 11)


# Conclusions -------------------------------------------------------------

# Filtering the data for variables that have a manageable level of missing values (<.25), are not geographic information or IDs, one value, pure linear combination of other variables, or highly correlated with other variables and we are left with 11 variables. After running the models and using BIC to determine the best number of variables we determine that the zestimate could be bettered from adding the following model: -7.19 * build_year + 1.59 * area_live_finished + -8.74 * num_bedrooms + 1.03 * region_county2061 +  1.17 * region_county3101 + -3.16 * zoning_landuse264 + 0 * zoning_landuse47 + 9.62 * zoning_landuse275. 

# We use the model derived from the Abs_logerror because it gives us a lower BIC indicator. This makes sense because the error is compounded as opposed to countered, meaning the errors are more obvious to the model.
