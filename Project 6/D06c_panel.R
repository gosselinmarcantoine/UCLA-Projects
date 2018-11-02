######################################################################################################
# D06c Panel Model
# By William Yu, UCLA Anderson Forecast
# 11/1/2018
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Data")

install.packages("foreign") 
install.packages("plm") 
library(foreign) 
library(car)
library(gplots)
library(plm)
library(Formula)

election <- data.frame(read_excel("W06c_election.xlsx"))  
plotmeans(Dvote~state, data=election)
plotmeans(Dvote~year, data=election)

# OLS
ols=lm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg, data=election)
summary(ols)

ols2=lm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg+Mhincomeg*D_Incumbent+Mhincomeg*R_Incumbent
        +Mhincomeg*Open_D_Previous, data=election)
summary(ols2)

# Fixed Effect
fixed1=plm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg+Mhincomeg*D_Incumbent+Mhincomeg*R_Incumbent
           +Mhincomeg*Open_D_Previous, index=c("state","year"), model="within",data=election)
summary(fixed1)

fixef(fixed1)

fixed2=lm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg+Mhincomeg*D_Incumbent+Mhincomeg*R_Incumbent
           +Mhincomeg*Open_D_Previous+factor(state), data=election)
summary(fixed2)

fixed3=lm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg+Mhincomeg*D_Incumbent+Mhincomeg*R_Incumbent
          +Mhincomeg*Open_D_Previous+factor(state)-1, data=election)   # remove the intercept
summary(fixed3)

# Choice b/w Fixed Effect and Pooling OLS
pFtest(fixed1, ols2)   # If p-value is <0.05, then the fixed effect model is betetr than OLS!

# Random Effect
random1=plm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg+Mhincomeg*D_Incumbent+Mhincomeg*R_Incumbent
           +Mhincomeg*Open_D_Previous, index=c("state","year"), model="random",data=election)
summary(random1)

# Choice b/w Fixed Effect and Random Effect
# Hausman Test
phtest(fixed1,random1) # If p-value is <0.05, then the fixed effect model is betetr than random effect!

# Time-fixed effect
tfe1=plm(Dvote~Nonwhite+Christian+Mhincome+Mhincomeg+Mhincomeg*D_Incumbent+Mhincomeg*R_Incumbent
            +Mhincomeg*Open_D_Previous+factor(year), index=c("state","year"), model="within", data=election)
summary(tfe1)

pFtest(tfe1,fixed1) # If p-value is <0.05, then the time-fixed effect model is betetr than fixed effect model!
