library(rKenyaCensus)
library(DT)
#library(tmap)
library(plotly)
library(tidyverse)
#library(sp)
library(sf)
#library(rgeos)
library(leaflet)
library(openxlsx)
#library(rgdal)
#pkgload::load_all(path= "rgdal_1.5-9/rgdal")

#library(rgdal)

options(scipen = 9999999)
DataCatalogue$Dataset <- gsub(" ","", DataCatalogue$Dataset) 
DataCatalogue <- rKenyaCensus::DataCatalogue %>% 
  mutate(Volume = ifelse(Dataset == "DataCatalogue", "DataCatalogue", as.character(Volume)))

dats <- unique(DataCatalogue$Dataset)
volumes <- unique(DataCatalogue$Volume)
counties <- unique(V4_T2.26$County)
DataCatalogue <- DataCatalogue %>% 
  mutate(VolumeDecription = ifelse(Volume == "V1", "Population by County and Sub-County",
                                  ifelse(Volume == "V2", "Distribution of Population by Administrative Units",
                                         ifelse(Volume == "V3", "Distribution of Population by Age, Sex and Administrative Units",
                                                ifelse(Volume == "V4", "Distribution of Population by Socio-Economic Characteristics","")))))

DataCatalogue <- DataCatalogue %>% 
  mutate(VolumeDecription =  ifelse(Dataset == "DataCatalogue", "xxx", as.character(VolumeDecription)))


V4_T2.33 <- rKenyaCensus::V4_T2.33
V4_T2.33 <- V4_T2.33 %>% select(-Total, -Male, -Female)
ICT_df <- full_join(V4_T2.32, V4_T2.33, by = c("County", "SubCounty", "AdminArea"))
# ICT_df$diff1 <- ICT_df$Total.x - ICT_df$Total.y
# ICT_df$diff2 <- ICT_df$Male.x - ICT_df$Male.y
# ICT_df$diff3 <- ICT_df$Female.x - ICT_df$Female.y
ict_vars<- names(ICT_df)[!names(ICT_df) %in% c("County","SubCounty","AdminArea")]

##-------------------------------------------

data <- ICT_df
data <- data %>% ungroup() %>% 
  mutate(County = as.character(County))%>% 
  filter(AdminArea== "County") 

Kenya_df <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)

KenyaCounties_SHP2 <- Kenya_df %>% st_transform(crs = 4326)

map <- full_join(KenyaCounties_SHP2, data, by="County")
