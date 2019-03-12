require(dplyr)
require(rgdal)
require(maps)
require(ggplot2)

dat <- read.csv("chap3_data.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

# MAPPING
# pick one species at a time, and convert UTMs to lat/longs
# need to separate by zone (3 zones) then bind back together
cofl <- dat4[dat4$species == "Caca", ]
cofl16 <- cofl[cofl$realUTMZone == 16, ]
cofl17 <- cofl[cofl$realUTMZone == 17, ]
cofl18 <- cofl[cofl$realUTMZone == 18, ]

# need to remove NAs before next step
cofl16 <- cofl16[!is.na(cofl16$realUTME), ]
cofl17 <- cofl17[!is.na(cofl17$realUTME), ]
cofl18 <- cofl18[!is.na(cofl18$realUTME), ]

# create UTM coordinates
cofl16utm <- SpatialPoints(cofl16[ , c("realUTME", "realUTMN")],
                           proj4string = CRS("+proj=utm +zone=16"))
cofl17utm <- SpatialPoints(cofl17[ , c("realUTME", "realUTMN")],
                           proj4string = CRS("+proj=utm +zone=17"))
cofl18utm <- SpatialPoints(cofl18[ , c("realUTME", "realUTMN")],
                           proj4string = CRS("+proj=utm +zone=18"))

# convert to lat/long
cofl16ll <- spTransform(cofl16utm, CRS("+proj=longlat"))
cofl17ll <- spTransform(cofl17utm, CRS("+proj=longlat"))
cofl18ll <- spTransform(cofl18utm, CRS("+proj=longlat"))

# create maps
# base map
map(database="state", regions=c("north carolina", "south carolina", "virginia", "tennessee", "georgia"))

# add points
points(cofl16ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(cofl17ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(cofl18ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
title(main = "Carpinus caroliniana locations")


# you can plot a simple measure of CNDDness
cofl$cndd <- cofl$plot_cons_tree_BA / cofl$sap_count
cofl$cndd[is.infinite(cofl$cndd)] <- NA

# map with color ramp for CNDDness
map.states <- map_data("state", region = c("north carolina", "south carolina"))

ggplot()  +
  geom_polygon(data = map.states, aes(x=long, y=lat, group=group), 
               fill = NA, color = "black") + 
  geom_point(data = cofl, aes(x=Public_Longitude, y=Public_Latitude, 
                              color=cndd), size = 3, alpha = 0.3) + 
  scale_colour_gradient(low = "blue", high = "red") + 
  theme_minimal()
