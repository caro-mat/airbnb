#data source
#https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data

library(tidyverse)

#import data

AB_NYC <- read.csv("data/AB_NYC_2019.csv", header=TRUE)
str(AB_NYC)
summary(AB_NYC)


#remove price 0

AB_NYC <-AB_NYC[AB_NYC$price > 0,]

#log price

AB_NYC <- cbind(AB_NYC,price_log = log(AB_NYC$price))
head(AB_NYC)

#remove inactive and make new dataset

AB_NYC_available <- AB_NYC %>% 
  filter(availability_365 > 0) 


#add distance to time square to model
#Times Square, Manhattan, NY, USA, Latitude and longitude coordinates are: 40.758896, -73.985130.
#https://cran.r-project.org/web/packages/geosphere/geosphere.pdf
library(geosphere)

help(package = geosphere)

coord <- cbind(AB_NYC_available$longitude,AB_NYC_available$latitude)

dist.timessquare <- distGeo(p1=coord, p2=c(-73.985130, 40.758896))

AB_NYC_available <- cbind(AB_NYC_available,dist.timessquare)
head(AB_NYC_available)
str(AB_NYC_available)

# Neighbourhood Group klein schreiben für Merging
AB_NYC_available$neighbourhood_group<-tolower(AB_NYC_available$neighbourhood_group)

#Leerzeichen aus den Distrikten entfernen
AB_NYC_available$neighbourhood_group <-gsub(" ","", AB_NYC_available$neighbourhood_group)

# Neighbourhood Group als Faktor
AB_NYC_available$neighbourhood_group<-factor(AB_NYC_available$neighbourhood_group)

#create subsets

AB_NYC_entirehome <-AB_NYC[AB_NYC_available$room_type == "Entire home/apt",]

AB_NYC_privateroom <-AB_NYC[AB_NYC_available$room_type == "Private room",]

AB_NYC_sharedroom <-AB_NYC[AB_NYC_available$room_type == "Shared room",]


