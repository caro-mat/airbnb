AB_NYC <- read.csv("data/AB_NYC_2019.csv", header=TRUE)
str(AB_NYC)
summary(AB_NYC)

#add distance to time square to model
#Times Square, Manhattan, NY, USA
#Latitude and longitude coordinates are: 40.758896, -73.985130.

#https://cran.r-project.org/web/packages/geosphere/geosphere.pdf

install.packages("geosphere")
library(geosphere)

help(package = geosphere)

coord <- cbind(AB_NYC$longitude,AB_NYC$latitude)

dist.timessquare <- distGeo(p1=coord, p2=c(-73.985130, 40.758896))

AB_NYC <- cbind(AB_NYC,dist.timessquare)
head(AB_NYC)
str(AB_NYC)