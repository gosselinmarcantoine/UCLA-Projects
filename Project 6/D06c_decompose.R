######################################################################################################
# D06b Time Series Decomposition
# By William Yu, UCLA Anderson Forecast
# 10/27/2018
##################################################################################################### 
# Reference; http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
install.packages("fpp2")
library(quantmod) 
library(tseries)
library(fpp2)

# Package "quantmod" can import data directly from various sources
# Real GDP Growth Rate; https://research.stlouisfed.org/fred2/series/A191RL1Q225SBEA/
getSymbols("A191RL1Q225SBEA", src="FRED")   # "getSymbols" is the function to download the data
plot(A191RL1Q225SBEA)   

getSymbols("DGS10", src="FRED") # 10-year Treasury Yield; https://research.stlouisfed.org/fred2/series/DGS10  
getSymbols("DGS2", src="FRED")  # 2-year Treasury Yield; https://research.stlouisfed.org/fred2/series/DGS2
spread=DGS10-DGS2
plot(spread)

getSymbols("RETAILSMSA", src="FRED")          # https://research.stlouisfed.org/fred2/series/RETAILSMSA
getSymbols("RETAILSMNSA", src="FRED")         # https://research.stlouisfed.org/fred2/series/RETAILSMNSA

retail <-ts(RETAILSMSA,frequency=12,start=c(1992,1))    # Seasonally adjusted data
retailn <-ts(RETAILSMNSA,frequency=12,start=c(1992,1))  # Non-seasonally adjusted data
ts.plot(retail, retailn, col=c("blue","red"))

ggseasonplot(retailn, year.labels=T, year.labels.left=T)
ggseasonplot(retailn, polar=T)
ggsubseriesplot(retailn)

# Decompostion of time series data
retail.comps = decompose(retailn)
plot(retail.comps)
plot(retail.comps$seasonal)                 
retail.sa = retailn - retail.comps$seasonal-retail.comps$random   # To get a seasonally adjusted data, a preliminary way   
ts.plot(retail, retail.sa, col=c("black","blue") )


