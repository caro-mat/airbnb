library(tidyverse)

#Einlesen
# https://data.cityofnewyork.us/City-Government/Agency-Performance-Mapping-Indicators-Annual/gsj6-6rwm
Ind_NYC<- read.csv("C:/Users/jonas/Studium HSLU/Module/R-Bootcamp/Project/Indicators_NYC.csv")
head(Ind_NYC)

#Nur noch Daten von 2019
Ind_NYC_2019<-data.frame("neighbourhood_group2"= Ind_NYC$Geographic.Identifier, "Indicator"=Ind_NYC$Indicator,"Incidents"=Ind_NYC$FY2019)
head(Ind_NYC_2019)
Ind_NYC_2019_cleaned<-Ind_NYC_2019


#Nummern aus den Distrikten entfernen
Ind_NYC_2019_cleaned$neighbourhood_group <-gsub("[0-9]","", Ind_NYC_2019_cleaned$neighbourhood_group2 )
#Leerzeichen aus den Distrikten entfernen
Ind_NYC_2019_cleaned$neighbourhood_group <-gsub(" ","", Ind_NYC_2019_cleaned$neighbourhood_group )
#Alle distrikte klein schreiben
Ind_NYC_2019_cleaned$neighbourhood_group<-tolower(Ind_NYC_2019_cleaned$neighbourhood_group)
# Neighbourhood Group als Faktor
Ind_NYC_2019_cleaned$neighbourhood_group<-factor(Ind_NYC_2019_cleaned$neighbourhood_group)

#Überblick
head(Ind_NYC_2019_cleaned$Incidents)  
head(Ind_NYC_2019_cleaned$neighbourhood_group)
summary(Ind_NYC_2019_cleaned)
summary(Ind_NYC_2019_cleaned$Indicator)
levels(Ind_NYC_2019_cleaned$neighbourhood_group)

# Summe der Incidents pro Distrikt und Indikator
Summary_Ind_NYC_2019<-Ind_NYC_2019_cleaned %>%
  group_by(neighbourhood_group=Ind_NYC_2019_cleaned$neighbourhood_group,Indicator) %>%
  summarise(Observations=sum(Incidents,na.rm = TRUE))
summary(Summary_Ind_NYC_2019)


# Angaben ohne Distrikt werte entfernen
Summary_Ind_NYC_2019<-filter(Summary_Ind_NYC_2019,neighbourhood_group != "")
summary(Summary_Ind_NYC_2019)
head(Summary_Ind_NYC_2019)

# Indikator verschachteln
NYC_nest<-Summary_Ind_NYC_2019 %>%
  nest(Indicator=c(Indicator, Observations))
head(NYC_nest)
