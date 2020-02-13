library(ggplot2)

## boxplots per room type, grouped by neighbourhood
ggplot(data = AB_NYC,
       mapping = aes(y = price_log,
                     x = "",
                     group = neighbourhood_group,
                     colour = neighbourhood_group)) +
  geom_boxplot() +
  facet_wrap(. ~ room_type)+
  xlab("")+
  ylab("log (price in $ per night)")


##simple linear models

lm.hood <- lm (data=AB_NYC, price_log~neighbourhood_group)
summary(lm.hood)

lm.type <- lm (data=AB_NYC, price_log~room_type)
summary(lm.type)

lm.dist <- lm (data=AB_NYC, price_log~dist.timessquare)
summary(lm.dist)

lm.dist.type.interact <- lm (data=AB_NYC, price_log~dist.timessquare+room_type)
summary(lm.dist.type.interact)

lm.dist.type <- lm (data=AB_NYC, price_log~dist.timessquare+room_type)
summary(lm.dist.type)


#multiple linear regression

lm.full <- lm (data=AB_NYC, price_log~room_type+neighbourhood_group+minimum_nights+number_of_reviews+calculated_host_listings_count+availability_365+dist.timessquare)
summary(lm.full)

lm.empty <- lm (data=AB_NYC, price_log~NULL)
add1(lm.empty,scope=lm.full)

#choose value with smallest RSS

lm.1 <- update(lm.empty,.~.+room_type) 
add1(lm.1,scope=lm.full)

lm.2 <- update(lm.1,.~.+dist.timessquare)
add1(lm.2,scope=lm.full)

lm.3 <- update(lm.2,.~.+availability_365)
add1(lm.3,scope=lm.full)

lm.4 <- update(lm.3,.~.+neighbourhood_group)
add1(lm.4,scope=lm.full)

lm.5 <- update(lm.4,.~.+minimum_nights)
add1(lm.5,scope=lm.full)

#lm by room type 

lm.full2 <- lm (data=AB_NYC_entirehome, price_log~neighbourhood_group+minimum_nights+number_of_reviews+calculated_host_listings_count+availability_365+dist.timessquare)
summary(lm.full2)

lm.full3 <- lm (data=AB_NYC_privateroom, price_log~neighbourhood_group+minimum_nights+number_of_reviews+calculated_host_listings_count+availability_365+dist.timessquare)
summary(lm.full3)

lm.full4 <- lm (data=AB_NYC_sharedroom, price_log~neighbourhood_group+minimum_nights+number_of_reviews+calculated_host_listings_count+availability_365+dist.timessquare)
summary(lm.full4)


