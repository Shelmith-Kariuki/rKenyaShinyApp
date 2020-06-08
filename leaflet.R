## install.packages("rgdal", repos="http://R-Forge.R-project.org")
library(tidyverse)
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(rKenyaCensus)

Kenya_df <- st_as_sf(KenyaCounties_SHP)
Kenya_df_tbl <- as_tibble(Kenya_df)
KenyaCounties_SHP <- KenyaCounties_SHP
proj4string(KenyaCounties_SHP)## Shows the projection. Kenya is in zone 37
## I need to understand about projections. 

## leaflet and so many packages work with long lat projection

## I need to change the projection of my file


KenyaCounties_SHP2 <- Kenya_df %>% st_transform(crs = 4326)

KenyaCounties_SHP2$Population <- as.numeric(as.character(KenyaCounties_SHP$Population))

#plot(KenyaCounties_SHP2)

## Vestor: polygons, line (road), points (shapefile is a vector)
## rastar: a matrix like remote sensing data
## sp is to sf as baseR is to tidyverse
leaflet(KenyaCounties_SHP2) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~colorQuantile("YlOrRd", Population)(Population))

#.dbf, .prj, .shp, .shx
KenyaCounties_SHP2$County <- as.character(KenyaCounties_SHP2$County)

dat2 <- full_join(KenyaCounties_SHP2, ICT_df, by="County")

pal <- colorBin(
  palette = "YlOrRd",
  domain = dat2$MPO_Total_Perc
)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  dat2$County, dat2$MPO_Total_Perc
) %>% lapply(htmltools::HTML)

leaflet(dat2) %>%
  addTiles() %>% 
  addPolygons(color = "red", weight = 1, dashArray = "3", fillColor = ~pal(MPO_Total_Perc),
              popup = paste(dat2$County, dat2$MPO_Total_Perc, sep=" : "),
              highlight = highlightOptions(
                weight = 4,
                color = "green",
                dashArray = "",
                bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
  addLegend(position = c("bottomright"),pal = pal, values = ~MPO_Total_Perc)


#my_map_wgs84 <- st_read("path/to/data.shp") %>% st_transform(crs = 4326)
