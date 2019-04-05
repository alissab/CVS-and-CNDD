require(dplyr)
require(rgdal)
require(maps)
require(ggplot2)

dat <- read.csv("chap3_hardw_plots_pca.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

# MAPPING
# convert UTMs to lat/longs
# need to separate by zone (3 zones) then bind back together
dat_plot <- dat[!duplicated(dat$Plot), ]
dat_plot <- dat_plot[!is.na(dat_plot$realUTMZone), ]
rownames(dat_plot) <- dat_plot$Plot

dat16 <- dat_plot[dat_plot$realUTMZone == 16, ]
dat17 <- dat_plot[dat_plot$realUTMZone == 17, ]
dat18 <- dat_plot[dat_plot$realUTMZone == 18, ]

# create UTM coordinates
dat16utm <- SpatialPoints(dat16[ , c("realUTME", "realUTMN")],
                           proj4string = CRS("+proj=utm +zone=16"))
dat17utm <- SpatialPoints(dat17[ , c("realUTME", "realUTMN")],
                           proj4string = CRS("+proj=utm +zone=17"))
dat18utm <- SpatialPoints(dat18[ , c("realUTME", "realUTMN")],
                           proj4string = CRS("+proj=utm +zone=18"))

# convert to lat/long
dat16ll <- spTransform(dat16utm, CRS("+proj=longlat"))
dat17ll <- spTransform(dat17utm, CRS("+proj=longlat"))
dat18ll <- spTransform(dat18utm, CRS("+proj=longlat"))



# create maps
# base map
par(mar=rep(0.5, 4))
map(database="state", regions=c("north carolina", "south carolina", 
                                "florida", "georgia"))

# add points
points(dat16ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(dat17ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(dat18ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
title(main = "CVS mixed hardwood plot locations, n = 1113 \n \n")






# map soil PCA axes 1 and 2 across plots to look for geographic trends
# need lat/long, plot, pca1, pca2, where each plot is unique
ll16 <- as.data.frame(dat16ll@coords)
ll17 <- as.data.frame(dat17ll@coords)
ll18 <- as.data.frame(dat18ll@coords)

ll16 <- cbind(ll16, dat16)
ll17 <- cbind(ll17, dat17)
ll18 <- cbind(ll18, dat18)

plot_pca <- rbind(ll16, ll17, ll18)
names(plot_pca)[1:2] <- c("x", "y")

par(mar = rep(0.5, 4))
basemap <- map_data(database="state", regions=c("north carolina", "south carolina"))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = pc1), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 0, low="red", mid="white",
                        high="blue", name="Soil PCA\nAxis 1") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = pc2), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 0, low="red", mid="white",
                        high="blue", name="Soil PCA\nAxis 2") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


plot_pca$twi_plot <- with(plot_pca, ifelse(
  twi>=300, 300, twi))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = twi_plot), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 130, low="red", mid="white",
                        high="blue", name="Total wetness\nindex") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = mean_prec), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 5, low="red", mid="white",
                        high="blue", name="Mean annual\nprecipitation") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = tmax), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 33, low="red", mid="white",
                        high="blue", name="Annual maximum\ntemperature") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


plot_pca$cons_plot <- with(plot_pca, ifelse(
  plot_cons_tree_BA>=5000, 5000, plot_cons_tree_BA))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = cons_plot), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 2000, low="red", mid="white",
                        high="blue", name="Conspecific\nbasal area") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


plot_pca$BA_plot <- with(plot_pca, ifelse(
  plot_tree_BA>=20000, 20000, plot_tree_BA))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plot_pca, mapping = aes(x = x, y = y, color = BA_plot), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 11000, low="red", mid="white",
                        high="blue", name="Conspecific\nbasal area") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())






# MAPPING plot locations by species
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





# you can plot a simple measure of CNDDness?





# to extract TOPOGRAPHIC MOISTURE INDEX data (.tif files) in ArcMap, need to give
# ArcMap lat/long values in table format
dat16_coords <- as.data.frame(dat16ll@coords)
dat17_coords <- as.data.frame(dat17ll@coords)
dat18_coords <- as.data.frame(dat18ll@coords)

dat16_coords$ID <- rownames(dat16_coords)
dat17_coords$ID <- rownames(dat17_coords)
dat18_coords$ID <- rownames(dat18_coords)

dat_plot$ID <- rownames(dat_plot)
dat_coords <- rbind(dat16_coords, dat17_coords, dat18_coords)
names(dat_coords) <- c("X", "Y", "ID")

write.csv(dat_coords, "chap3_data_coords_for_arcgis.csv", row.names = FALSE)
