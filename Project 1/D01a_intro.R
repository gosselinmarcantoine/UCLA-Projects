#####################################################################################################
# D01 Introduction to R  
# By William Yu, UCLA Anderson Forecast
# updated 9/28/2018
##################################################################################################### 

# R Scripts
# Open an existing R script: File -> Open File... and then go to where the R script is located
# Create a new R script: File -> New File -> R Script

#
# Basic Computation.  After # are comments lines in green color  
# Highlight the command and click "Run" or place the cursor on the command line and hit "Ctrl"+"Enter"
#
help(mean)
?sd
?rnorm

2+3
# assign "=" "<-" are identical and interchangable
a = (3*8/4)^2
a

a1 = (7-1)^3-9/2
a1
a2 <- ((7-1)^3-9)/2
a2

b <- log(100)
b

exp(b)

3angeles = sqrt(49)     # Variables cannot begin with a number
angeles = sqrt(49)
angeles
Angeles                 # R is case sensitive
test = 1,800            # No comma!!!
test = 1800
test

# Construct vector 
x=c(1,3,5,8)
x
x^2
y=c(-1:12)
y
length(y)               # The length of the data


# Construct matrix and data management
y1=matrix(0,4,5)        # A 4 rows by 5 columns matrix of zeros
y1
y2=matrix(1:10,nrow=2)  # A 2 by 5 matrix
y2
y3=matrix(1:10,ncol=5)
y3
y4=y2+y3
y4
y5=t(y4)                # The transpose of 2 by 5 becomes a matrix of 5 by 2
y5
y6=as.vector(y5)
y6
y7=as.vector(t(y5))
y7

dim(y5)
nrow(y5)
ncol(y5)

# Matrix elements
y5[3,2]                 # The third row and the second column
y5[3,]                  # The third row
y5[,2]                  # The second column

# Logical value: TRUE(T) and FALSE(F)
# Logical operator: >,<,>=,<=,!=
x1= 1:6
x1
x1<5
y5<= 6
?subset

# Generate random number with standard normal distribution
x2=rnorm(5000)
x2

plot(x2)
plot(x2, type="l")      # Plot type is "line"
hist(x2, breaks=40)
?hist
x3=rnorm(1000, mean=10, sd=0.5)
mean(x3)
var(x3)
sqrt(var(x3))
sd(x3)

par(mfrow=c(1,2))
hist(x2)
hist(x3)
par(mfrow=c(1,1))
?is.na

# Previous command: up arrow

rm(list=ls()) # Clear up the global enviroment

# Data cleaning is very important for handling dirty data, in particular for missing value!!
x = c(1,2,3,NA, 4)
x
is.na(x)
mean(x)
mean(x, na.rm=TRUE) # na.rm: Should missing values be removed from the calculation ("TRUE" or "T")

# Replace non-numeric values to clean it up for numerical analysis

# Problems
test=c("$1000","$2,000","3,000",4000, "5000  ")
test
mean(test)
class(test)

# Solutions: gsub(A,B,data) is a useful substitution function. Replace A with B.  

test=gsub(",","",test)  # Replace "," with "", meaning to remove "," in test.
test
test=gsub("\\$","",test)
test
class(test)
test=as.numeric(test)
test
class(test)
mean(test)

######################################################################################################
# Install packages and import data from Yahoo Finance
######################################################################################################
install.packages("tseries")       # You only need to install package for the first time
library(tseries)                  # You need to call on library every time you restart the R script

# Import the data from Yahoo finance directly: Chnage instrument for different stocks
sp500 = get.hist.quote(instrument="^gspc", start="1976-01-01", end="2018-08-01", quote="AdjClose", 
                       provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")

msft = get.hist.quote(instrument="msft", start="1976-01-01", end="2018-08-01", quote="AdjClose", 
                      provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")

wmt = get.hist.quote(instrument="wmt", start="1976-01-01", end="2018-08-01", quote="AdjClose", 
                      provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")

par(mfrow=c(2,2))       # Make mutiple chart display 2 rows by 2 columns
plot(sp500)
plot(msft)
plot(wmt)
par(mfrow=c(1,1))       # Change chart setting back to the default
barplot(sp500)

class(sp500)            # Check the object class of data, zoo is one kind of time series object      
sp500[1:5,]             # See first five rows of the data

# Frequency Coversion: Monthly data to Quarterly data
# First, use "ts" function to convert the series to another form of time series
sp500.month <- ts(sp500, start=c(1976,1), frequency=12)
class(sp500.month)
sp500.quarter <- aggregate(sp500.month, nfrequency=4,sum) # Another way is sum
sp500.quarter

write.csv(sp500.quarter, "sp500.quarter.csv")   # export your data


# Generate continuosly compounded returns from stock prices: log(Pt)-log(Pt-1)
?diff
sp500.r=diff(log(sp500))
msft.r=diff(log(msft))
wmt.r=diff(log(wmt))

plot(sp500.r,main="S&P 500 Stock Returns",xlab="Year",ylab="returns")  # main="" is for the title of the chart
abline(h=mean(sp500.r))  # horizontal line is the S&P500 return mean. v= for vertical line.
summary(sp500.r)

# Useful data magament tool
stocks = cbind(sp500.r,msft.r,wmt.r)            # Combine columns;  Alternative:cbind.data.frame
colnames(stocks) <- c("SP500", "MSFT", "WMT")   # Change the column names
stocks[1:10,]         # Show the first 5 rows
plot(stocks, xlab="Monthly Returns")
colMeans(stocks)
colMeans(stocks, na.rm=T)   
summary(stocks)
stocks1=na.exclude(stocks)  # na.exclude() function returns the data with incomplete row removed.
                            # na.omit() function is the same as na.exclude
stocks1[1:5,]

# Show smoothed histogram of the data
hist(sp500.r)
plot(density(sp500.r),xlim=c(-0.4,0.4), main="Smoothed Histogram of Monthly Stock Returns",lty=1)
points(density(msft.r), type="l", lty=1, lwd=5, col="red")  #lty is line style, lwd is line width
points(density(wmt.r), type="l", lty=1, lwd=2, col="blue")
legend(-0.4, 10, legend=c("S&P500", "MSFT","WMT"), lty=c(1,2,3), col=c("black","red","blue"))

pairs(stocks)
# Calculate correlation
cor(stocks)
cor(stocks, use="complete.obs")   # use="complete.obs" handle missing value

# 3-dimenion plots
rm(list=ls()) # Clear up the global enviroment
?contour
?persp
x=seq(-20,20)
y=x
z=outer(x,y, function(x,y)3*x^2+y^2)
contour(x,y,z)
persp(x,y,z)

persp(x,y,z,theta=30,phi=20)
persp(x,y,z,theta=40,phi=20)
persp(x,y,z,theta=50,phi=20)
persp(x,y,z,theta=60,phi=20)

# Update R version. Better Do it in R.
install.packages("installr") 
library(installr) 

updateR() 

