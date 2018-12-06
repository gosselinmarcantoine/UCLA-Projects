#####################################################################################################
# Research Project for Anderson Joshoson Johnson Head Start Program
# created on May 1, 2017, revised on September 27, 2018
##################################################################################################### 

#
# Import data from your computer:
#
# (1) Easy way:
#
# Click Tools --> Import dataset --> From Text File... -->    Find your data in your computer. 
# Note that it only can import csv file not xls file. So if the data is xls file, you need to convert it to csv file.
# In R Console, you will see the following result: 
# > @#$ <- read.csv("~/Zip08/2018 Q4 Fall_XData/Data/W01b_headstart_pre.xlsx")
# You have successfully import the data into R 
#
# (2) Professional way:
#
# First Change the working directory where the data are for your own computer!!
# Click the top bar of your file. For PC, it reads "C:\Users\WYU\Documents\Zip08\2018 Q4 Fall_XData\Data" backward slash
# Note that in R, it should be changed to forward slash: setwd("C:/.../..../..")
# Note that maybe Mac users' start with setwd("/.../..../..")

# Check your current working directory!!
getwd()
# Set your working directory in R
setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Data")

#
# Data Import  
#
# If the imported data are not csv files, you can either (1) convert them to csv files or (2): 
# install.packages("readxl")                        # Importing xls,xlsx files is slower than read.csv. 
# library(readxl)                                   # So I don't recomemnd this way for importing big data.
# @#$ <- read_excel("@#$.xlsx")

install.packages("readxl")  # You only need to install it for the first time
library(readxl)             # You need to run this everytime to import the data in xlsx form

# Import the Date file into data frame
# By default, it will only import the data in the first tab, e.g. Sheet1 
pre <- read_excel("W01b_headstart_pre.xlsx")  
post <- read_excel("W01c_headstart_post.xlsx")  

colnames(pre)
# agency, age, race, insurance, info
# do.fever, do.vomit, do.earche, do.cough, sick.where, bettercare

tpr.agency=as.data.frame(table(pre$agency))
tpr.agency$Perc <- tpr.agency$Freq/sum(tpr.agency$Freq)*100
tpo.agency=as.data.frame(table(post$agency))
tpo.agency$Perc <- tpo.agency$Freq/sum(tpo.agency$Freq)*100
t.agency=merge(tpr.agency, tpo.agency, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.agency[,2:5], na.rm=T)))
t.agency <- rbind(t.agency, total)
colnames(t.agency) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.age=as.data.frame(table(pre$age))
tpr.age$Perc <- tpr.age$Freq/sum(tpr.age$Freq)*100
tpo.age=as.data.frame(table(post$age))
tpo.age$Perc <- tpo.age$Freq/sum(tpo.age$Freq)*100
t.age=merge(tpr.age, tpo.age, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.age[,2:5], na.rm=T)))
t.age <- rbind(t.age, total)
colnames(t.age) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

pre$race1 <- gsub("Other: .*","Other",pre$race)
post$race1 <- gsub("Other: .*","Other",post$race)

tpr.race=as.data.frame(table(pre$race1))
tpr.race$Perc <- tpr.race$Freq/sum(tpr.race$Freq)*100
tpo.race=as.data.frame(table(post$race1))
tpo.race$Perc <- tpo.race$Freq/sum(tpo.race$Freq)*100
t.race=merge(tpr.race, tpo.race, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.race[,2:5], na.rm=T)))
t.race <- rbind(t.race, total)
colnames(t.race) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.insurance=as.data.frame(table(pre$insurance))
tpr.insurance$Perc <- tpr.insurance$Freq/sum(tpr.insurance$Freq)*100
tpo.insurance=as.data.frame(table(post$insurance))
tpo.insurance$Perc <- tpo.insurance$Freq/sum(tpo.insurance$Freq)*100
t.insurance=merge(tpr.insurance, tpo.insurance, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.insurance[,2:5], na.rm=T)))
t.insurance <- rbind(t.insurance, total)
colnames(t.insurance) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.info.book=as.data.frame(table(pre$info.book))
tpr.info.book$Perc <- tpr.info.book$Freq/sum(tpr.info.book$Freq)*100
tpo.info.book=as.data.frame(table(post$info.book))
tpo.info.book$Perc <- tpo.info.book$Freq/sum(tpo.info.book$Freq)*100
t.info.book=merge(tpr.info.book, tpo.info.book, by="Var1", all=TRUE)
t.info.book=t.info.book[2,]

tpr.info.internet=as.data.frame(table(pre$info.internet))
tpr.info.internet$Perc <- tpr.info.internet$Freq/sum(tpr.info.internet$Freq)*100
tpo.info.internet=as.data.frame(table(post$info.internet))
tpo.info.internet$Perc <- tpo.info.internet$Freq/sum(tpo.info.internet$Freq)*100
t.info.internet=merge(tpr.info.internet, tpo.info.internet, by="Var1", all=TRUE)
t.info.internet=t.info.internet[2,]

tpr.info.tv=as.data.frame(table(pre$info.tv))
tpr.info.tv$Perc <- tpr.info.tv$Freq/sum(tpr.info.tv$Freq)*100
tpo.info.tv=as.data.frame(table(post$info.tv))
tpo.info.tv$Perc <- tpo.info.tv$Freq/sum(tpo.info.tv$Freq)*100
t.info.tv=merge(tpr.info.tv, tpo.info.tv, by="Var1", all=TRUE)
t.info.tv=t.info.tv[2,]

tpr.info.family=as.data.frame(table(pre$info.family))
tpr.info.family$Perc <- tpr.info.family$Freq/sum(tpr.info.family$Freq)*100
tpo.info.family=as.data.frame(table(post$info.family))
tpo.info.family$Perc <- tpo.info.family$Freq/sum(tpo.info.family$Freq)*100
t.info.family=merge(tpr.info.family, tpo.info.family, by="Var1", all=TRUE)
t.info.family=t.info.family[2,]

tpr.info.doctor=as.data.frame(table(pre$info.doctor))
tpr.info.doctor$Perc <- tpr.info.doctor$Freq/sum(tpr.info.doctor$Freq)*100
tpo.info.doctor=as.data.frame(table(post$info.doctor))
tpo.info.doctor$Perc <- tpo.info.doctor$Freq/sum(tpo.info.doctor$Freq)*100
t.info.doctor=merge(tpr.info.doctor, tpo.info.doctor, by="Var1", all=TRUE)
t.info.doctor=t.info.doctor[2,]

tpr.info.know=as.data.frame(table(pre$info.know))
tpr.info.know$Perc <- tpr.info.know$Freq/sum(tpr.info.know$Freq)*100
tpo.info.know=as.data.frame(table(post$info.know))
tpo.info.know$Perc <- tpo.info.know$Freq/sum(tpo.info.know$Freq)*100
t.info.know=merge(tpr.info.know, tpo.info.know, by="Var1", all=TRUE)
t.info.know=t.info.know[2,]

t.info <- rbind(t.info.book, t.info.internet, t.info.tv, t.info.family, t.info.doctor, t.info.know )
colnames(t.info) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 
t.info$Var1 <-c("I look in a health book or magazine","I find it on Internet", "I find it on TV","I ask my family or friends",
                "Get information from the doctor or clinic","Just know how to take care of my child")

tpr.do.fever=as.data.frame(table(pre$do.fever))
tpr.do.fever$Perc <- tpr.do.fever$Freq/sum(tpr.do.fever$Freq)*100
tpo.do.fever=as.data.frame(table(post$do.fever))
tpo.do.fever$Perc <- tpo.do.fever$Freq/sum(tpo.do.fever$Freq)*100
t.do.fever=merge(tpr.do.fever, tpo.do.fever, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.do.fever[,2:5], na.rm=T)))
t.do.fever <- rbind(t.do.fever, total)
colnames(t.do.fever) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.do.vomit=as.data.frame(table(pre$do.vomit))
tpr.do.vomit$Perc <- tpr.do.vomit$Freq/sum(tpr.do.vomit$Freq)*100
tpo.do.vomit=as.data.frame(table(post$do.vomit))
tpo.do.vomit$Perc <- tpo.do.vomit$Freq/sum(tpo.do.vomit$Freq)*100
t.do.vomit=merge(tpr.do.vomit, tpo.do.vomit, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.do.vomit[,2:5], na.rm=T)))
t.do.vomit <- rbind(t.do.vomit, total)
colnames(t.do.vomit) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.do.earche=as.data.frame(table(pre$do.earche))
tpr.do.earche$Perc <- tpr.do.earche$Freq/sum(tpr.do.earche$Freq)*100
tpo.do.earche=as.data.frame(table(post$do.earche))
tpo.do.earche$Perc <- tpo.do.earche$Freq/sum(tpo.do.earche$Freq)*100
t.do.earche=merge(tpr.do.earche, tpo.do.earche, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.do.earche[,2:5], na.rm=T)))
t.do.earche <- rbind(t.do.earche, total)
colnames(t.do.earche) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.do.cough=as.data.frame(table(pre$do.cough))
tpr.do.cough$Perc <- tpr.do.cough$Freq/sum(tpr.do.cough$Freq)*100
tpo.do.cough=as.data.frame(table(post$do.cough))
tpo.do.cough$Perc <- tpo.do.cough$Freq/sum(tpo.do.cough$Freq)*100
t.do.cough=merge(tpr.do.cough, tpo.do.cough, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.do.cough[,2:5], na.rm=T)))
t.do.cough <- rbind(t.do.cough, total)
colnames(t.do.cough) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.sick.where=as.data.frame(table(pre$sick.where))
tpr.sick.where$Perc <- tpr.sick.where$Freq/sum(tpr.sick.where$Freq)*100
tpo.sick.where=as.data.frame(table(post$sick.where))
tpo.sick.where$Perc <- tpo.sick.where$Freq/sum(tpo.sick.where$Freq)*100
t.sick.where=merge(tpr.sick.where, tpo.sick.where, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.sick.where[,2:5], na.rm=T)))
t.sick.where <- rbind(t.sick.where, total)
colnames(t.sick.where) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

tpr.bettercare=as.data.frame(table(post$bettercare))
tpr.bettercare$Perc <- tpo.bettercare$Freq/sum(tpo.bettercare$Freq)*100
tpo.bettercare=as.data.frame(table(post$bettercare))
tpo.bettercare$Perc <- tpo.bettercare$Freq/sum(tpo.bettercare$Freq)*100
t.bettercare=merge(tpr.bettercare, tpo.bettercare, by="Var1", all=TRUE)
total=t(c(Var1="Total", colSums(t.bettercare[,2:5], na.rm=T)))
t.bettercare <- rbind(t.bettercare, total)
colnames(t.bettercare) <- c("Var1", "Pre N", "(%)", "Post N", "(%)") 

hs1.all=rbind(NA,NA,NA, t.agency,NA,NA,NA,t.age, NA,NA,NA,t.race,NA,NA,NA,t.insurance,NA,NA,NA,t.info,NA,NA,NA,NA,t.do.fever,
                 NA,NA,NA,t.do.vomit,NA,NA,NA,t.do.earche,NA,NA,NA,t.do.cough,NA,NA,NA,t.sick.where,
                 NA,NA,NA,t.bettercare)

write.csv(hs1.all,"hs1.all.csv")










