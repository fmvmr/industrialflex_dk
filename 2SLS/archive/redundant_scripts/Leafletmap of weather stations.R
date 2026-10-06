# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
##### Leaflet map for weather stations: 
# Packages
install.packages(c("dplyr", "leaflet"), quiet = TRUE)
library(dplyr)
library(leaflet)

# ---------------------------
# 1) Create station dataframe
# ---------------------------

stations <- tibble::tibble(
  zone = c(rep("DK1", 10), rep("DK2", 10)),
  stationId = c(
    "06118","06093","06088","06104","06102","06039","06072","06049","06051","06032",
    "06141","06154","06136","06135","06170","06180","06186","06156","06181","06169"
  ),
  name = c(
    "Sønderborg Lufthavn","Vester Vedsted","Nordby","Billund Lufthavn","HORSENS/BYGHOLM",
    "Galten","Ødum","Hald Vest","Vestervig","Stenhøj",
    "Abed","Brandelev","Tystofte","Flakkebjerg","Roskilde Lufthavn",
    "Københavns Lufthavn","Landbohøjskolen","Holbæk","Jægersborg","Gniben"
  ),
  lat = c(
    54.9616,55.2908,55.4483,55.7379,55.8680,56.1618,56.3027,56.5604,56.7637,57.3828,
    54.8275,55.2075,55.2465,55.3224,55.5867,55.6140,55.6814,55.7154,55.7664,56.0067
  ),
  lon = c(
    9.7930,8.6551,8.4003,9.1674,9.7872,9.9033,10.1272,10.0929,8.3207,10.3349,
    11.3292,11.8605,11.3285,11.3879,12.1366,12.6455,12.5403,11.7088,12.5263,11.2805
  )
)

print(stations)

# ---------------------------
# 2) Leaflet map
# ---------------------------

pal <- colorFactor(palette = c("blue", "red"), domain = stations$zone)

leaflet(stations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 6,
    color = ~pal(zone),
    stroke = TRUE, weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0("<b>", name, "</b><br>",
                    "Zone: ", zone, "<br>",
                    "Station ID: ", stationId, "<br>",
                    "Lat: ", lat, " | Lon: ", lon)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~zone,
    title = "Price Zone",
    opacity = 1
  )
leaflet(stations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 6,
    color = ~pal(zone),
    stroke = TRUE, weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0("<b>", name, "</b><br>",
                    "Zone: ", zone, "<br>",
                    "Station ID: ", stationId, "<br>",
                    "Lat: ", lat, " | Lon: ", lon)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~zone,
    title = "Price Zone",
    opacity = 1
  )
