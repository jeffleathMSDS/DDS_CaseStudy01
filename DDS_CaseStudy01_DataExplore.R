## Ganesh and Jeff
## DDS Case Study 1, Data Exploration
## test edit to see that I can push it back to github

## Load packages
library(plyr)
library(tidyr)
library(dplyr)
library(base)
library(ggplot2)

## read in brewery csv file
brewery = read.csv("Source/Breweries.csv", header = TRUE)
str(brewery)
## change names in file
names(brewery) <-c("Brewery_id","Brewery_Name", "City", "State")
str(brewery)


## read in beers csv file
beers = read.csv("Source/beers.csv", header = TRUE)
str(beers)

## Answer Question 1:  Brewery count by state
table(brewery$State)

## Merge files
newtable <- merge(beers, brewery, by="Brewery_id")
str(newtable)

##export merged table
write.table(newtable, file="merged_file.csv",sep=",",row.names=T)

## summary stats for merged table
summary(newtable)

## Q2: Show first 6
head(newtable,6)


## Q3: Report the number of NA's in each column.
na_count <-sapply(newtable, function(y) sum(length(which(is.na(y)))))
na_count

## Q4:  Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

## calc median ABV by state
medianABV <-aggregate(ABV ~ State, newtable, median)

## bar plot for median ABV
countABV <- table(medianABV$ABV)
barplot(countABV, main="Median ABV by State", 
        xlab="ABV")

## calc median IBU by state
medianIBU <-aggregate(IBU ~ State, newtable, median)

## bar plot for median IBU
countIBU <- table(medianIBU$IBU)
barplot(countIBU, main="Median IBU by State", 
        xlab="IBU")

## Q5:  Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

## table of max ABV by state
maxABV <-aggregate(ABV ~ State, newtable, max)
## sort
maxABV <-maxABV[order(-maxABV$ABV),]
## Top 5 results
head(maxABV,5)

## table of max IBU by state
maxIBU <-aggregate(IBU ~ State, newtable, max)
## sort
maxIBU <-maxIBU[order(-maxIBU$IBU),]
## Top 5 results
head(maxIBU,5)



## Q6:  Summary statistics for the ABV variable.
summary(newtable$ABV)


