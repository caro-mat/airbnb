library(tidyverse)

# Beide Datensets kontrollieren
head(AB_NYC_available)
head(NYC_nest)

#Join both datasets
NYC<-left_join(AB_NYC_available,NYC_nest, by="neighbourhood_group")
str(NYC)
# Neighbourhood Group als Faktor
NYC$neighbourhood_group<-factor(NYC$neighbourhood_group)
str(NYC)
