### Jane S. Rogosch
### Created 7 May 2021
### This code is to match fish locations to nearest USGS discharge data from gages in xeric ecoregion

### relevant links:
# https://pakillo.github.io/R-GIS-tutorial/
# https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/
# https://stackoverflow.com/questions/25411251/buffer-geospatial-points-in-r-with-gbuffer
# https://engineerpaige.com/how-to-calculate-the-distance-along-a-polyline-in-r/
# https://stackoverflow.com/questions/28922874/should-be-easy-distance-along-a-line-in-r
# https://www.jessesadler.com/post/gis-with-r-intro/
# https://cengel.github.io/rspatial/2_spDataTypes.nb.html
# http://www.nickeubank.com/gis-in-r/
# https://geocompr.robinlovelace.net/reproj-geo-data.html

# Load libraries--------------------------------------------------------
source("Code/extract_USGS_gage_data.R")
library(dplyr)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
#library(geosphere)
library(rworldmap)

# Get data-------------------------------------------------------------
# Gage Locations
head(gageLocation)

# Fish locations
# Fish sample locations
fishLocation <- read.csv("Data/Xeric_Points_All.csv") # datum is WGSM 84
head(fishLocation)

# Import stream flowlines or do by radius first?-------------------
# Plot points - limit locations to USA
#?distm # only find distances within a matrix, not between matrices
plot(gageLocation$dec_long_va, gageLocation$dec_lat_va)
plot(fishLocation$lon, fishLocation$lat)
min(gageLocation$dec_lat_va)
USA_fishLocation <- fishLocation[fishLocation$lon <= -90 
                                 & fishLocation$lat >= 30 
                                 & fishLocation$source != "fishsprich", ]



plot(gageLocation$dec_long_va, gageLocation$dec_lat_va)
points(USA_fishLocation$lon, USA_fishLocation$lat, pch = 19, col = "green3")

# Define spatial projection and make spatial data objects
# consult with http://www.spatialreference.org/
fishCoords <- dplyr::select(USA_fishLocation, lon, lat)
gageCoords <- dplyr::select(gageLocation, dec_long_va, dec_lat_va)

points_fish <- SpatialPoints(coords = fishCoords, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

unique(gageLocation$dec_coord_datum_cd)
points_gage <- SpatialPoints(coords = gageCoords, proj4string = CRS("+proj=longlat +datum=NAD83 +ellps=WGS84"))

plot(points_fish, pch = 20, col="steelblue")
?rworldmap
plot(coastsCoarse, add = T)
points(points_gage)

# A projection mercator epsg:3857 / US national atlas equal area epsg:2163
?spTransform
points_fish_tr <- spTransform(points_fish, CRS( "+init=epsg:3857" ) ) 
points_gage_tr <- spTransform(points_gage, CRS( "+init=epsg:3857" ) ) 


# Create a buffer around fish points to limit gages
?gBuffer
fish_buffer <- gBuffer(points_fish_tr, width = 50000) # I think width is in meters for this projection (need to double check)

plot(points_fish_tr, pch = 20, col="red")
plot(fish_buffer, border = "#008080", add = T)
points(points_gage_tr)

gage_subset <- points_gage_tr[fish_buffer, ] #wooh! that was easy. now i just need to bring along identifiers
points(gage_subset, col = "orange")

# Next import flowlines 
# perhaps also clip to buffer, and then calculate distance from fish site to nearest gage for final U.S. gage subset
# Import flowlines
#readOGR for vecotr, dsn = datasource name (path to folder) layer = layer name without extension
RioGrande_lines <- readOGR(dsn = "C:/Users/jrogosch/OneDrive - Texas Tech University/Documents/ArcGIS/NHDPlusv2/NHDPlusV21_RG_13_NHDSnapshot_05/NHDPlusRG/NHDPlus13/NHDSnapshot/Hydrography",
                           layer = "NHDFlowline")

names(RioGrande_lines)
plot(RioGrande_lines)
plot(coastsCoarse, add = T)
points(points_gage, col = "red")
points(points_fish, pch = 20, col="steelblue")

#Subset sites for trial run - crashing. need to get on same CRS first I guess 
# Try spTransform() example:
# proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# crs_args <- NLCD@crs@projargs
# sites_transformed <- spTransform(coords, CRS(crs_args))
## crs_args <- RioGrande_lines@crs@projargs


RioGrande_gagesubset <- points_gage[RioGrande_lines, ]
plot(RioGrande_lines)
points(RioGrande_gagesubset, pch = 20, col="steelblue")

RioGrande_fishsubset <- points_fish[RioGrande_lines, ]
proj4string(RioGrande_lines)
proj4string(points_fish)
proj4string(points_gage)



