head(NYC)
library("tidyverse")
library("leaflet")


#dataframe for the most expensive and cheapest housings
df_exp<-filter(NYC,price == max(price))
df_cheap<-filter(NYC,price == min(price))



getColor <- function(NYC) {
  sapply(NYC$price, function(price) {
    if(price <= 50) {
      "green"
    } else if(price <= 200) {
      "orange"
    } else {
      "red"
    } })
}

getGroup <- function(NYC) {
  sapply(NYC$price, function(price) {
    if(price <= 50) {
      "Cheap"
    } else if(price <= 200) {
      "Medium"
    } else {
      "Expensive"
    } })
}


map<-leaflet(NYC) %>%
  #Base
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$CartoDB.Positron,group="GREY")%>%
  
  # Overlay groups
  addCircleMarkers(lng = NYC$longitude, lat = NYC$latitude, color=getColor(NYC),radius=0.1,group = getGroup(NYC)) %>%
  
  #LayerCOntrol
  addLayersControl(baseGroups = c("OSM","GREY"), overlayGroups = c("Cheap","Medium","Expensive"))


map

castle<-makeIcon(
  iconUrl = "C:/Users/jonas/Studium HSLU/Module/R-Bootcamp/Project/castle.png",
  iconWidth = 20, iconHeight = 20)

map2<-leaflet(NYC) %>%
  #Base
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$CartoDB.Positron,group="GREY")%>%
  
  # Overlay groups
  addMarkers(lng=df_exp$longitude, lat=df_exp$latitude, icon= castle, popup = "Most expensive Airbnb Housing", group=("Most expensive Airbnb Housing")) %>%
  addMarkers(lng=df_cheap$longitude, lat=df_cheap$latitude, icon= castle, popup = "Cheapest Airbnb Housing", group=("Cheapest Airbnb Housing")) %>%
  #LayerCOntrol
  addLayersControl(baseGroups = c("OSM","GREY"), overlayGroups = c("Most expensive Airbnb Housing","Cheapest Airbnb Housing"))

map2

