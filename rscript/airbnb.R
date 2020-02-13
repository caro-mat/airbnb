#https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data


AB_NYC <- read.csv("C:/Users/CaroleM/Downloads/new-york-city-airbnb-open-data/AB_NYC_2019.csv", header=TRUE)
str(AB_NYC)
summary(AB_NYC)

library(ggplot2)

## boxplots per room type, grouped by neighbourhood
ggplot(data = AB_NYC,
       mapping = aes(y = price,
                     x = "",
                     group = neighbourhood_group,
                     colour = neighbourhood_group)) +
  geom_boxplot() +
  ylim(0,500)+
  facet_wrap(. ~ room_type)+
  xlab("")+
  ylab("price in $ per night")

## boxplots by room type

ggplot(data = AB_NYC,
       mapping = aes(y = price,
                     x = "",
                     group = room_type,
                     colour = room_type)) +
  geom_boxplot() +
  ylim(0,1000)


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

##simple linear models

lm.hood <- lm (data=AB_NYC, price~neighbourhood_group)
summary(lm.hood)

lm.type <- lm (data=AB_NYC, price~room_type)
summary(lm.type)

lm.dist <- lm (data=AB_NYC, price~dist.timessquare)
summary(lm.dist)
plot(data = AB_NYC, price~dist.timessquare,ylim=1000)
abline(lm.dist, col="red")

#multiple linear regression

lm.full <- lm (data=AB_NYC, price~room_type+neighbourhood_group+room_type+price+minimum_nights+number_of_reviews+calculated_host_listings_count+availability_365+dist.timessquare)
summary(lm.full)

lm.empty <- lm (data=AB_NYC, price~NULL)
add1(lm.empty,scope=lm.full)

#choose value with smallest RSS

lm.1 <- update(lm.empty,.~.+room_type)
add1(lm.1,scope=lm.full)

lm.2 <- update(lm.1,.~.+neighbourhood_group)
add1(lm.2,scope=lm.full)

lm.3 <- update(lm.2,.~.+number_of_reviews)
add1(lm.3,scope=lm.full)

lm.4 <- update(lm.3,.~.+availability_365)
add1(lm.4,scope=lm.full)

lm.5 <- update(lm.4,.~.+dist.timessquare)
add1(lm.5,scope=lm.full)

summary(lm.5)
plot(lm.5)

###########################################
###########################################

#remove all high prices 

summary(AB_NYC)
AB_NYC_lowprice <- AB_NYC[AB_NYC$price <= 500,]

#full linear model for prices below 500

lm.full <- lm (data=AB_NYC_lowprice, price~room_type+neighbourhood_group+room_type+price+minimum_nights+number_of_reviews+calculated_host_listings_count+availability_365+dist.timessquare)
summary(lm.full)

lm.empty <- lm (data=AB_NYC_lowprice, price~NULL)
add1(lm.empty,scope=lm.full)

#choose value with smallest RSS

lm.1 <- update(lm.empty,.~.+room_type)
add1(lm.1,scope=lm.full)

lm.2 <- update(lm.1,.~.+dist.timessquare)
add1(lm.2,scope=lm.full)

lm.3 <- update(lm.2,.~.+availability_365)
add1(lm.3,scope=lm.full)

lm.4 <- update(lm.3,.~.+number_of_reviews)
add1(lm.4,scope=lm.full)

lm.5 <- update(lm.4,.~.+neighbourhood_group)
add1(lm.5,scope=lm.full)

summary(lm.5)

##simple linear models for low prices vs. distance to timessquare
##including subsets

AB_NYC_lowprice_entirehome <- AB_NYC_lowprice[AB_NYC_lowprice$room_type == "Entire home/apt",]

lm.dist.low.entirehome <- lm (data=AB_NYC_lowprice_entirehome, price~dist.timessquare)
summary(lm.dist.low.entirehome)
plot(data = AB_NYC_lowprice_entirehome, price~dist.timessquare)
abline(lm.dist.low.entirehome, col="red")

## more ggplot

ggplot(data = AB_NYC_lowprice,
       mapping = aes(y = price,
                     x = dist.timessquare,
                     color = neighbourhood_group))+
  geom_point(alpha = 0.5)+
  facet_wrap(. ~ room_type)






