## Ganesh and Jeff
## DDS Case Study 1, Data Exploration

## Load packages
library(plyr)
library(tidyr)
library(dplyr)
library(base)
library(ggplot2)

## read in brewery csv file
brewery = read.csv("Breweries.csv", header = TRUE)
str(brewery)
## change names in file
names(brewery) <-c("Brewery_id","Brewery_Name", "City", "State")
str(brewery)


## read in beers csv file
beers = read.csv("beers.csv", header = TRUE)
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

## need bar plot


## calc median IBU by state
medianIBU <-aggregate(IBU ~ State, newtable, median)

## need bar plot

## Q5:  Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?


## Q6:  Summary statistics for the ABV variable.
summary(newtable$ABV)

