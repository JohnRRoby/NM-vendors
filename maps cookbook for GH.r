# code for maps with the vendors story


library(geojsonio)
library(leaflet)
library(tidyverse)
library(htmltools)
library(htmlwidgets)

#read in state-level data drawn from the main file
stateSum <- read.csv("stateSum.csv", stringsAsFactors = F)

#use an r built-in constant to replace abbreviation w name, then tolower to match format in states db
stateSum$STATE <- state.name[match(stateSum$STATE,state.abb)]

#fix table to add DC and PR, and remove USVI row (sorry USVI)
stateSum[8,1] <- "District of Columbia"
stateSum[39,1] <- "Puerto Rico"
stateSum <- stateSum[-c(47),]

#rename the STATE column to NAME to match the states object
colnames(stateSum)[1] <- "NAME"

#formatting the spending variable
stateSum$spending <- stateSum$spending / 1000000
stateSum$spending <- format(round(stateSum$spending, 3), nsmall = 3)

#do the merge. this must use the MERGE function from pkg sp to work, or will throw a regular data.frame
require(sp)
states <- merge(states, stateSum, by = "NAME")

#ensure var stays numeric and prettify the vendors count
states$spending <- as.numeric(states$spending)
states$count <- format(states$count, big.mark = ",")


#
#
#
# build the base map
#
#
states <- geojson_read("us_states2.geojson", what = "sp")

#remove New Mexico
states <- subset(states, NAME != "New Mexico")

#make the bins and assign color spread to them
bins <- c(0, 25, 50, 100, 200, 400, 650)
pal <- colorBin(c("lightgoldenrod", "palegreen2", "tan1", "turquoise2", "skyblue4", 
"brown3", "firebrick4"), domain = states$spending, bins = bins)

#style the labels
labels <- sprintf(
  "<strong>%s</strong><br/>
  New Mexico does business with %s vendors in %s<br/>
  and has spent $%5.3f million there since 2013.", #c stuff: %5.3f means 13 digits, 0 decimals, f is dbl precision numeric
  states$NAME, states$count, states$NAME, states$spending
) %>% lapply(htmltools::HTML)


m <- leaflet(states, options = leafletOptions(minZoom = 3, maxZoom = 7)) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  #use CartoDB.Positron as basemap
  addPolygons(
  fillColor = ~pal(spending),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal, values = ~spending, opacity = 0.7, title = "New Mexico's out-of-state spending <br/>in millions of dollars",
  position = "bottomright")



#
# map of in-state spending by ZIP
#
#

library(geojsonio)
library(leaflet)
library(tidyverse)
library(htmltools)
library(htmlwidgets)

#read in the zip csv
zipcodes <- read.csv("zipcodes.csv", stringsAsFactors = F)

#read in the json zipcode data
zips <- geojson_read("us_zctas_2017.geojson", what = "sp")

#change zips from numeric to factor
zipcodes$ZCTA5CE10 <- as.factor(zipcodes$ZCTA5CE10)


#rename the zip column to match the states object
colnames(zipcodes)[1] <- "ZCTA5CE10"

#formatting the spending variable
zipcodes$spending <- zipcodes$spending / 1000000
zipcodes$spending <- format(round(zipcodes$spending, 4), nsmall = 4)

#do the merge. this must use the MERGE function from pkg sp to work, or will throw a regular data.frame
require(sp)
zips <- merge(zips, zipcodes, by = "ZCTA5CE10")


#subset the json data
zips <- subset(zips, state == "new mexico")

# prettify the counts
zips$count <- comma(zips$count)




#make the bins and assign color spread to them
bins <- c(0, 0.5, 5, 10, 50, 100, 500)
pal <- colorBin(c("lightgoldenrod", "palegreen2", "tan1", "turquoise2", "skyblue4", 
"brown3", "firebrick4"), domain = zips$spending, bins = bins)

#style the labels
labels <- sprintf(
  "<strong>%s</strong><br/>
  New Mexico has %s vendors in ZIP Code %s<br/>
  and has spent $%6.6s million there since 2013.", #c stuff: %5.3f means 5 digits, 4 decimals, f is dbl precision numeric
  zips$ZCTA5CE10, zips$count, zips$ZCTA5CE10, zips$spending
) %>% lapply(htmltools::HTML)


m <- leaflet(zips, options = leafletOptions(minZoom = 7, maxZoom = 9)) %>%
  setView(-106, 34.6, 7) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  #addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
  fillColor = ~pal(spending),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7, 
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.9,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal, values = ~spending, opacity = 0.7, 
  title = "New Mexico's in-state spending <br/>by ZIP code in millions of dollars",
  position = "bottomright")
