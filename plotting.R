require(dplyr)
require(rgdal)
require(maps)
require(ggplot2)

dat <- read.csv("chap3_data_by_plot.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

# MAPPING
# convert UTMs to lat/longs
# need to separate by zone (3 zones) then bind back together
dat_plot <- dat[!duplicated(dat$Plot), ]
dat_plot <- dat_plot[!is.na(dat_plot$realUTMZone), ]

dat16 <- dat_plot[dat_plot$realUTMZone == 16, ]
dat17 <- dat_plot[dat_plot$realUTMZone == 17, ]
dat18 <- dat_plot[dat_plot$realUTMZone == 18, ]

# need to remove NAs before next step
dat16 <- dat16[!is.na(dat16$realUTME), ]
dat17 <- dat17[!is.na(dat17$realUTME), ]
dat18 <- dat18[!is.na(dat18$realUTME), ]

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
title(main = "CVS plot locations, n = 1943")


# find loblolly and longleaf plots for Bob to look at
longleaf_bob <- as.data.frame(dat[grep("longleaf", dat$commPrimaryCommon, ignore.case = TRUE), 
                    "commPrimaryCommon"])
longleaf_bob <- distinct(longleaf_bob)
names(longleaf_bob) <- "comm"
loblolly_bob <- as.data.frame(dat[grep("loblolly", dat$commPrimaryCommon, ignore.case = TRUE), 
                    "commPrimaryCommon"])
loblolly_bob <- distinct(loblolly_bob)
names(loblolly_bob) <- "comm"
for_bob <- rbind(longleaf_bob, loblolly_bob)

# find maritime plots for Bob
maritime <- as.data.frame(dat[grep("maritime", dat$commPrimaryCommon, ignore.case = TRUE), 
                              "commPrimaryCommon"])
maritime <- distinct(maritime)
names(maritime) <- "comm"

# find peatlands
peat <- as.data.frame(dat[grep("peat", dat$commPrimaryCommon, ignore.case = TRUE), 
                          "commPrimaryCommon"])
peat <- distinct(peat)
names(peat) <- "comm"

# find pocosin
pocosin <- as.data.frame(dat[grep("pocosin", dat$commPrimaryCommon, ignore.case = TRUE), 
                             "commPrimaryCommon"])
pocosin <- distinct(pocosin)
names(pocosin) <- "comm"

# find swamps
swamp <- as.data.frame(dat[grep("swamp", dat$commPrimaryCommon, ignore.case = TRUE), 
                           "commPrimaryCommon"])
swamp <- distinct(swamp)
names(swamp) <- "comm"

# find coastal fringe plots
fringe <- as.data.frame(dat[grep("fringe", dat$commPrimaryCommon, ignore.case = TRUE), 
                            "commPrimaryCommon"]) 
fringe <- distinct(fringe)
names(fringe) <- "comm"

# bind unwanted plots together
for_bob <- rbind(for_bob, maritime, peat, pocosin, swamp, fringe)
write.csv(for_bob, "CVS_communities_to_remove.csv", row.names = FALSE)

# make map of plots that you want to remove
plots_remove <- dat[dat$commPrimaryCommon %in% for_bob$comm, ]
plots_remove <- plots_remove[!duplicated(plots_remove$Plot), ]
plots_remove <- plots_remove[!is.na(plots_remove$realUTMZone), ]

plots_remove16 <- plots_remove[plots_remove$realUTMZone == 16, ]
plots_remove17 <- plots_remove[plots_remove$realUTMZone == 17, ]
plots_remove18 <- plots_remove[plots_remove$realUTMZone == 18, ]

# create UTM coordinates
plots_remove16utm <- SpatialPoints(plots_remove16[ , c("realUTME", "realUTMN")],
                          proj4string = CRS("+proj=utm +zone=16"))
plots_remove17utm <- SpatialPoints(plots_remove17[ , c("realUTME", "realUTMN")],
                          proj4string = CRS("+proj=utm +zone=17"))
plots_remove18utm <- SpatialPoints(plots_remove18[ , c("realUTME", "realUTMN")],
                          proj4string = CRS("+proj=utm +zone=18"))

# convert to lat/long
plots_remove16ll <- spTransform(plots_remove16utm, CRS("+proj=longlat"))
plots_remove17ll <- spTransform(plots_remove17utm, CRS("+proj=longlat"))
plots_remove18ll <- spTransform(plots_remove18utm, CRS("+proj=longlat"))

# create maps
# base map
par(mar=rep(0.5, 4))
map(database="state", regions=c("north carolina", "south carolina", 
                                "florida", "georgia"))

# add points
points(plots_remove16ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(plots_remove17ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(plots_remove18ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
title(main = "CVS plots to remove, n = 388")




# map after you remove plots







# remove loblolly and longleaf pine plots
longleaf_plots <- dat[grep("longleaf", dat$commPrimaryCommon, ignore.case = TRUE), 
                   c("Plot", "commPrimaryCommon")]
longleaf_plots <- distinct(longleaf_plots)
loblolly_plots <- dat[grep("loblolly", dat$commPrimaryCommon, ignore.case = TRUE), 
                      c("Plot", "commPrimaryCommon")]
loblolly_plots <- distinct(loblolly_plots)

lobl <- dat %>% filter(commPrimaryCommon ! %in% )



# MAPPING plot locations by species
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
