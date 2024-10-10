
# Data Preparation file

#Loading required libraries

library(tidyverse)
library(vroom)
library(scales)
library(glue)
library(janitor)
library(sf)
library(rgdal)
library(htmltools)
library(leaflet)
library(utf8)
rm(list=ls())

#pak_shpOGR <- readOGR("C:/Users/ZHaider/Desktop/DOCS_WB/Rshiny_Projects/Pakistan_indicators/Pak_Districts_SHP/District_Boundary.shp")
#pak_shpOGR <- readOGR("C:/Users/ZHaider/Desktop/DOCS_WB/Rshiny_Projects/Pakistan_indicators/PAK_indicators_SHP/pakistan_indicators.shp")
pak_shp <- st_read("C:/Users/ZHaider/Desktop/DOCS_WB/Rshiny_Projects/Pakistan_indicators/PAK_indicators_SHP/pakistan_indicators.shp")
pak_ind <- read_delim("C:/Users/ZHaider/Desktop/DOCS_WB/Rshiny_Projects/Pakistan_indicators/pakistan_indicators.csv", delim=",", na = c("", "NA"))

#Checking if names of districts match in both datasets
dist_shp <- unique(pak_shp$District)
dist_ind <- unique(pak_ind$District)
is.element(dist_ind, dist_shp)

# GB, AJK and Kashmir arn't given in the .shp file , so dropping these from indicators dataset for consistency in the join later
#4308 rows aren't matching up due to these 3 locations.


#Pivoting long which is easier to work with
pak_ind <- pak_ind %>% 
  filter(!District %in% c("Gilgit Baltistan", "Jammu & Kashmir", "AJ&K")) %>% 
  pivot_longer(-(Province:Year), names_to = "indicator", values_to = "value") %>% 
  clean_names()
#i.e furterh data cleaning on the file and adding domain and source in it


#data %>% group_by(year) %>% count(district, sort=T) 
#data %>% group_by(year, indicator) %>% count(district) %>% filter(n!=1)
#Chitral entry  repeats twice in 2019, data entry error (146*2)

#data %>% group_by(year) %>%  count(district) %>% filter(n != 146) # only chitral is an error for 2019.

chitral <- pak_ind %>% 
  filter(district == "Chitral" & year == 2019) %>% 
  slice(row_number(1:146))

#Since Kachhi (Balochistan) District is missing in 2019. I'm puttin it in with NA value for all indicators so coorect values popup on resprective districts in 2019
kachhi <-  chitral %>%    #using chitral obs to create the same with NAs for Kachhi
  mutate(province = "Balochistan",
         district = "Kachhi",
         value = NA)

#Pakistan Indicators file  
#Mutating the domain class of each indicator so user can select first domain than respective indicator

pak_ind <- pak_ind %>% 
  filter(!(district == "Chitral" & year == 2019)) %>% 
  rbind(chitral) %>% 
  rbind(kachhi) %>% 
  mutate_if(is.character, utf8_encode) %>% 
  arrange(province, district) 

#Reading in csv for domain names and source and definitions
domain <- read_csv("C:/Users/ZHaider/Desktop/DOCS_WB/Rshiny_Projects/Pakistan_indicators/domain_source_pak_indicators.csv", na = c("", "")) 
  
domain <- domain %>% 
  mutate(indicator_1 = str_to_title(indicator_1),
         units = replace_na(units,""))

#Joining domain + source + definition with the data
pak_ind <- left_join(pak_ind, domain, by="indicator")
pak_ind <- pak_ind %>% 
  select(province:year, indicator_1, units, domain:definition, context, indicator, value)

#Creating a new variable for year+ source filtering
pak_ind <- pak_ind %>% 
  mutate(year_1 = year, source_1 = source) %>% 
  unite(year_1, c("year_1", "source_1"), sep="-")

#*******************************************This is the file being used in the app of Pakistan_indicatros
saveRDS(pak_ind, "pak_ind.RDS")
# ***************************************************************************************
# ************************************************************************



#Cleaning shape file, keeping only 2018 since leftjoin is repeating matches otherwise. 
#We are only interested in geometry of districts anyways so no difference
# pak_shp <- pak_shp %>% 
#   clean_names() %>% 
#   filter(year == 2018) %>% 
#   select(province, district, p_pop, geometry)

#*
#pak_shp <- pak_shpOGR[pak_shpOGR$Year == 2018, ]

#merging geodata
# Rows match perfectly now
# pak_ind_shp <- inner_join(pak_ind, pak_shp , by = c("province", "district"))  

#*
#pak_ind_shp <- left_join(pak_ind, pak_shpOGR , by = c("District"), copy = T)  



# saveRDS(pak_ind_shp, file= "DATA.RDS")   #81MB is quite managable in the App

# data <- readRDS("DATA.RDS")

# d1 <- data %>% 
#   filter(indicator == 'Poverty Rate (%)',
#          year == 2014) %>%  
#          st_as_sf()   #convert into sf dataframe


# labels <-  sprintf(
#   "<strong>%s</strong><br/> Poverty Rate",
#   d1$district, 
#   d1$value) %>% 
#   lapply(htmltools::HTML)
# 
# labels <- paste("District: ", d1$district, "<br/>", "Poverty Rate: ", round(d1$value, 2), sep= "") %>% 
#   lapply(htmltools::HTML)


# pal <- colorBin(palette = "OrRd", bins= 6, domain= d1$value)
# 
# 
# map_interactive <-  st_as_sf(pak_shp) %>% 
#   
#   st_transform(crs = "+init=epsg:4326") %>%  
#   leaflet() %>% 
#   addProviderTiles(providers$CartoDB.Positron,
#                    options = tileOptions(minZoom = 0,
#                                          maxZoom = 13),
#                    group = "OpenStreetMap") %>% 
#   addPolygons(label= labels,
#               stroke = FALSE,
#               smoothFactor = 0.5,
#               opacity = 1,
#               fillOpacity = 0.7,
#               fillColor = ~pal(d1$value),
#               highlightOptions = highlightOptions(weight= 5,
#                                                   fillOpacity = 1,
#                                                   color="black",
#                                                   opacity= 1,
#                                                   bringToFront = TRUE  
#               )) %>% 
#   addLegend("bottomright",
#             pal=pal,
#             values= ~d1$value,
#             title = "Poverty Rate (%)",
#             opacity= 0.7) %>% 
#   addMeasure()
# 
# 
# map_interactive
# 
#   
# 
