##################################################################################################
# P01
# Script Answer to Project 1, UCLA Extension Data Science Intensive
# on 9/30/2018 by William Yu
##################################################################################################

#
# A
#
x=465600
z=0.045
y=30
c=z/12
n=y*12
p=x*(c*(1+c)^n)/((1+c)^n-1)
p

# Alternative output
cat("The monthly payment is", p)
cat("The monthly payment is $", p)                # Not ideal, $ and amount is not together
cat(paste("The monthly payment is $", p,sep=""))  # Paste $ and amount together
p=round(p,2)                                      # Round the output to decimal two
cat(paste("The monthly payment is $", p,sep=""))

# Alternative Way: Function
mortgage=function(price,downpay,interest,year){
  x=price*(1-downpay/100)
  z=interest/100
  c=z/12
  n=year*12
  p=x*(c*(1+c)^n)/((1+c)^n-1)
  return(paste("The monthly payment is $", round(p,2),sep=""))
 }
mortgage(582000,20,4.5,30)

#
# B
#
library(readxl)            

setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Project/project1")

laz2017 <- data.frame(read_excel("P01_LA zipcode payroll.xlsx", sheet="2017")) 

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)

colnames(laz17tech) = c("zipcode", "total", "information", "professional") 
laz17tech$per = (laz17tech$information + laz17tech$professional) / laz17tech$total

write.csv(laz17tech,"laz17tech.csv")








