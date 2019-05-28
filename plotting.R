
require(rgdal)
require(maps)
require(dplyr)
require(ggplot2)
require(ggExtra)  # for marginal histograms
require(ggmap)
require(sjPlot)
require(sjstats)
require(gridExtra)
require(bayesplot)


dat <- read.csv("chap3_hardw_plots_USE_10April.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

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
map(database="state", regions=c("north carolina", "south carolina"))

# add points
points(dat16ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(dat17ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
points(dat18ll, pch=21, cex=2, col="black", bg=rgb(red=0, green=1, blue=0, alpha=0.3))
title(main = "CVS mixed hardwood plot locations, n = 1093 \n \n")


# trying to get base topographic map (elevation) with plot locations
# but not working well
require(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addPolygons()




# map soil PCA axes 1 and 2 across plots to look for geographic trends
# need lat/long, plot, pca1, pca2, where each plot is unique
ll16 <- as.data.frame(dat16ll@coords)
ll17 <- as.data.frame(dat17ll@coords)
ll18 <- as.data.frame(dat18ll@coords)

ll16 <- cbind(ll16, dat16)
ll17 <- cbind(ll17, dat17)
ll18 <- cbind(ll18, dat18)

plots <- rbind(ll16, ll17, ll18)
names(plots)[1:2] <- c("x", "y")

par(mar = rep(0.5, 4))
basemap <- map_data(database="state", regions=c("north carolina", "south carolina"))



ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(pc1)), mapping = aes(x = x, y = y, color = pc1), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 0.025, low="red", mid="white",
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
  geom_point(data = subset(plots, !is.na(pc2)), mapping = aes(x = x, y = y, color = pc2), 
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


# modify twi value to help with map interpretation
plots$twi_plot <- with(plots, ifelse(
  twi>=300, 300, twi))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plots, mapping = aes(x = x, y = y, color = twi_plot), 
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
  geom_point(data = plots, mapping = aes(x = x, y = y, color = mean_prec), 
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
  geom_point(data = plots, mapping = aes(x = x, y = y, color = tmax), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 33, low="blue", mid="white",
                        high="red", name="Annual maximum\ntemperature") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# make cons BA easier to interpret
plots$cons_plot <- with(plots, ifelse(
  plot_cons_tree_BA>=5000, 5000, plot_cons_tree_BA))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plots, mapping = aes(x = x, y = y, color = cons_plot), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 2000, low="blue", mid="white",
                        high="red", name="Conspecific\nbasal area") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# help with map interpretation
plots$BA_plot <- with(plots, ifelse(
  plot_tree_BA>=20000, 20000, plot_tree_BA))

ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = plots, mapping = aes(x = x, y = y, color = BA_plot), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient2(midpoint = 11000, low="blue", mid="white",
                        high="red", name="Total tree\nbasal area") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())




# PLOTTING PARAMETER ESTIMATES
# PLOT RANDOM EFFECTS
# species:con vs. species:het

ran <- ranef(full_con_het)
ran$species[,,1]  # extracts random intercept values for each species[1:3]
ran$species[,,2]  # plot_het_tree_BA
ran$species[,,3]  # plot_cons_tree_BA

# ran$species[species names, column estimates, random slope]
ran$species[1:3, , ]  # extracts all random slopes for each species[1:3]

# require(sjstats)
cons_betas <- as.data.frame(ran$species[,, 3])  # random slopes of conspBA for all species
cons_betas$term <- row.names(cons_betas)

# heterospecific random slope by species
het_betas <- as.data.frame(ran$species[,, 2])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(cons_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-2.1, 1.5)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red") +
  ggtitle("Effect of conspecific basal area\n ") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))



ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-2.1, 1.5)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.5) +
  ggtitle("Effect of heterospecific basal area\n ") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))




# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# tmax is random effect #4, 5 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 5])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 4])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.9, 0.6)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of interaction between \nconspecific basal area and temperature") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.9, 0.6)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and temperature") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))







# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# mean_prec is random effect #6, 7 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 7])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 6])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.4, 0.4)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of interaction between \nconspecific basal area and precipitation") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.4, 0.4)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and precipitation") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))





# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# twi is random effect #8, 9 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 9])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 8])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.35, 0.55)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of interaction between \nconspecific basal area and wetness") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.35, 0.55)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and wetness") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))




# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# PC1 is random effect #10, 11 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 11])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 10])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.75, 0.55)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of interaction between \nconspecific basal area and soil PCA axis 1") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.75, 0.55)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and soil PCA axis 1") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))




# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# pc2 is random effect 12, 13 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 13])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 12])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.8, 0.6)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of interaction between \nconspecific basal area and soil PCA axis 2") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.8, 0.6)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and soil PCA axis 2") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))





# new model testing effects of elevation

# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# Elevation is random effect 12, 13 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 5])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 4])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.8, 1)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of interaction between \nconspecific basal area and elevation") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.8, 1)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and elevation") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))



# PLOTTING FIXED EFFECTS

# plot posterior distributions
# require(bayesplot)
post <- as.array(full_con_het)
color_scheme_set("gray")
post.plot <- mcmc_areas(post, pars = c("b_plot_cons_tree_BA", "b_plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  coord_cartesian(ylim = c(1.5, 2)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())


post.plot <- mcmc_areas(post, pars = c("b_twi:plot_cons_tree_BA", "b_twi:plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  coord_cartesian(ylim = c(1.5, 2.5)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(rep(0.5, 4), "cm"))




post.plot <- mcmc_areas(post, pars = c("b_pc1:plot_cons_tree_BA", "b_pc1:plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  coord_cartesian(ylim = c(1.5, 2.5)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(rep(0.5, 4), "cm"))





post.plot <- mcmc_areas(post, pars = c("b_pc2:plot_cons_tree_BA", "b_pc2:plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  coord_cartesian(ylim = c(1.5, 2.5)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(rep(0.5, 4), "cm"))





post.plot <- mcmc_areas(post, pars = c("b_tmax:plot_cons_tree_BA", "b_tmax:plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  coord_cartesian(ylim = c(1.5, 2.5)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(rep(0.5, 4), "cm"))




post.plot <- mcmc_areas(post, pars = c("b_mean_prec:plot_cons_tree_BA", "b_mean_prec:plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  coord_cartesian(ylim = c(1.5, 2.5)) +
  scale_x_continuous(limits=c(-0.15, 0.1)) +#, breaks=seq(-0.15, 0.15, 0.05)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(rep(0.5, 4), "cm"))



post.plot <- mcmc_areas(post, pars = c("b_Elevation:plot_cons_tree_BA", "b_Elevation:plot_het_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")

post.plot + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) + 
  scale_x_continuous(limits = c(-0.8, 1)) +
  coord_cartesian(ylim = c(1.5, 2.1)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=20),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())



# overall interaction between basal area and TWI (con vs. het)
twi <- marginal_effects(mod, effects = "plot_cons_tree_BA:twi")
twi <- as.data.frame(twi$`plot_cons_tree_BA:twi`)
twi <- twi %>% select(plot_cons_tree_BA, twi, estimate__, se__, lower__, upper__)

p <- ggplot(data = dat, aes(x = plot_cons_tree_BA, y = sap_plot_count)) + 
  scale_x_continuous(minor_breaks = seq(0, 8, 2)) + 
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 60, 10), 
                     minor_breaks = seq(0, 60, 10)) +
  geom_point(data = dat, aes(x = plot_cons_tree_BA, y = sap_plot_count), alpha = 0) +
  geom_ribbon(data = twi[twi$twi == -1, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_cons_tree_BA),
              alpha=0.3, linetype = "dotted", size=1.2, color="gray30", fill = "gray30") +
  geom_ribbon(data = twi[twi$twi == 1.03, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_cons_tree_BA),
              alpha=0, linetype = "longdash", size=1, color="black") +

  geom_smooth(data = twi[twi$twi == -1, ], inherit.aes=FALSE,
              aes(x = plot_cons_tree_BA, y = estimate__), 
              size=1.2, linetype = "dotted", color="gray30", method="lm") +
  geom_smooth(data = twi[twi$twi == 1.03, ], inherit.aes=FALSE,
              aes(x = plot_cons_tree_BA, y = estimate__), 
              size=1, linetype = "longdash", color="black", method="lm") +
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.margin = unit(c(5,5,5,5), "mm"))

ggMarginal(p, type = "histogram")
  





twi.h <- marginal_effects(mod, effects = "plot_het_tree_BA:twi")
twi.h <- as.data.frame(twi.h$`plot_het_tree_BA:twi`)
twi.h <- twi.h %>% select(plot_het_tree_BA, twi, estimate__, se__, lower__, upper__)

p.h <- ggplot(data = dat, aes(x = plot_het_tree_BA, y = sap_plot_count)) + 
  scale_x_continuous(minor_breaks = seq(-2.5, 7.5, 2.5)) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 60, 10),
                     minor_breaks = seq(0, 60, 10)) +
  geom_point(data = dat, aes(x = plot_het_tree_BA, y = sap_plot_count), alpha = 0) +
  geom_ribbon(data = twi.h[twi.h$twi == -1, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_het_tree_BA),
              alpha=0.3, linetype = "dotted", size=1.2, color="gray30", fill = "gray30") +
  geom_ribbon(data = twi.h[twi.h$twi == 1.03, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_het_tree_BA),
              alpha=0, linetype = "longdash", size=1, color="black") +
  
  geom_smooth(data = twi.h[twi.h$twi == -1, ], inherit.aes=FALSE,
              aes(x = plot_het_tree_BA, y = estimate__), 
              size=1.2, linetype = "dotted", color="gray30", method="lm") +
  geom_smooth(data = twi.h[twi.h$twi == 1.03, ], inherit.aes=FALSE,
              aes(x = plot_het_tree_BA, y = estimate__), 
              size=1, linetype = "longdash", color="black", method="lm") +
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.margin = unit(c(5,5,5,5), "mm"))

ggMarginal(p.h, type = "histogram")




# overall interaction between basal area and PC1

pc1 <- marginal_effects(mod, effects = "plot_cons_tree_BA:pc1")
pc1 <- as.data.frame(pc1$`plot_cons_tree_BA:pc1`)
pc1 <- pc1 %>% select(plot_cons_tree_BA, pc1, estimate__, se__, lower__, upper__)

p <- ggplot(data = dat, aes(x = plot_cons_tree_BA, y = sap_plot_count)) + 
  scale_x_continuous(minor_breaks = seq(0, 8, 2)) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 60, 10),
                     minor_breaks = seq(0, 60, 10)) +
  geom_point(data = dat, aes(x = plot_cons_tree_BA, y = sap_plot_count), alpha = 0) +
  geom_ribbon(data = pc1[pc1$pc1 == -1, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_cons_tree_BA),
              alpha=0.3, linetype = "dotted", size=1.2, color="gray30", fill = "gray30") +
  geom_ribbon(data = pc1[pc1$pc1 == 1, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_cons_tree_BA),
              alpha=0, linetype = "longdash", size=1, color="black") +
  
  geom_smooth(data = pc1[pc1$pc1 == -1, ], inherit.aes=FALSE,
              aes(x = plot_cons_tree_BA, y = estimate__), 
              size=1.2, linetype = "dotted", color="gray30", method="lm") +
  geom_smooth(data = pc1[pc1$pc1 == 1, ], inherit.aes=FALSE,
              aes(x = plot_cons_tree_BA, y = estimate__), 
              size=1, linetype = "longdash", color="black", method="lm") +
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.margin = unit(c(5,5,5,5), "mm"))

ggMarginal(p, type = "histogram")






pc1.h <- marginal_effects(mod, effects = "plot_het_tree_BA:pc1")
pc1.h <- as.data.frame(pc1.h$`plot_het_tree_BA:pc1`)
pc1.h <- pc1.h %>% select(plot_het_tree_BA, pc1, estimate__, se__, lower__, upper__)

p.h <- ggplot(data = dat, aes(x = plot_het_tree_BA, y = sap_plot_count)) + 
  scale_x_continuous(minor_breaks = seq(-2.5, 7.5, 2.5)) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 60, 10),
                     minor_breaks = seq(0, 60, 10)) +
  geom_point(data = dat, aes(x = plot_het_tree_BA, y = sap_plot_count), alpha = 0) +
  geom_ribbon(data = pc1.h[pc1.h$pc1 == -1, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_het_tree_BA),
              alpha=0.3, linetype = "dotted", size=1.2, color="gray30", fill = "gray30") +
  geom_ribbon(data = pc1.h[pc1.h$pc1 == 1, ], inherit.aes=FALSE,
              aes(ymin = lower__, ymax = upper__, x = plot_het_tree_BA),
              alpha=0, linetype = "longdash", size=1, color="black") +
  
  geom_smooth(data = pc1.h[pc1.h$pc1 == -1, ], inherit.aes=FALSE,
              aes(x = plot_het_tree_BA, y = estimate__), 
              size=1.2, linetype = "dotted", color="gray30", method="lm") +
  geom_smooth(data = pc1.h[pc1.h$pc1 == 1, ], inherit.aes=FALSE,
              aes(x = plot_het_tree_BA, y = estimate__), 
              size=1, linetype = "longdash", color="black", method="lm") +
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.margin = unit(c(5,5,5,5), "mm"))

ggMarginal(p.h, type = "histogram")





# REPORTING QUANTITATIVE RESULTS
require(tidyr)
require(brms)
post <- as.array(full_con_het)  # array: [# iterations, # chains, # parameters]

# gather all iterations across all chains together into one column for each 
# predictor of interest, which gives you its posterior distribution

# only the sapling count response needs the neg. binom. distribution; for all
# predictors, you can use normal
sap <- data.frame(post[, , "b_Intercept"])
sap <- c(t(sap))  # if you want a vector of all the pulls from the posterior
mcmc_areas(post, pars = "b_Intercept", prob = 0.8, prob_outer = 0.95, point_est = "mean")
pnbinom(q = 1.5, size = 0.55, mu = mean(sap))  # probabilities you want to report
# q = quantile you're interested in; size = shape parameter; mu = mean of posterior


twi_BA <- data.frame(post[,,"b_twi:plot_cons_tree_BA"])
twi_BA <- c(t(twi_BA))
mcmc_areas(post, pars = "b_twi:plot_cons_tree_BA", prob = 0.8, prob_outer = 0.95, point_est = "mean")
qnorm(p = 0.025, mean = mean(twi_BA), sd = sd(twi_BA))
# 95% lower quartile: 0.049464

twi_het <- data.frame(post[,,"b_twi:plot_het_tree_BA"])
twi_het <- c(t(twi_het))
mcmc_areas(post, pars = "b_twi:plot_het_tree_BA", prob = 0.8, prob_outer = 0.95, point_est = "mean")
pnorm(q = 0.049464, mean = mean(twi_het), sd = sd(twi_het))
# there is a 61.8% chance that TWI:het_tree_BA is <= the lower 95% quartile of TWI:cons_tree_BA



pc1_het <- data.frame(post[,,"b_pc1:plot_het_tree_BA"])
pc1_het <- c(t(pc1_het))
qnorm(p = 0.025, mean = mean(pc1_het), sd = sd(pc1_het))

pc1_BA <- data.frame(post[,,"b_pc1:plot_cons_tree_BA"])
pc1_BA <- c(t(pc1_BA))
pnorm(q = -0.09403784, mean = mean(pc1_BA), sd = sd(pc1_BA))
# there is a 36.5% chance that pc1:cons_tree_BA is <= the lower 95% quartile of pc1:het_tree_BA



# INTERSPECIFIC CONSPECIFIC DD EFFECTS (POSITIVE AND NEGATIVE)
# POSITIVE
ilde <- data.frame(post[,,"r_species[Ilde,plot_cons_tree_BA]"])
ilde <- c(t(ilde))
qnorm(p = 0.025, mean = mean(ilde), sd = sd(ilde))
# 95% lower quartile: 0.2911469

ilde_het <- data.frame(post[,,"r_species[Ilde,plot_het_tree_BA]"])
ilde_het <- c(t(ilde_het))
pnorm(q = 0.2911469, mean = mean(ilde_het), sd = sd(ilde_het))
# there is a 99.0% chance that Ilde-het_tree_BA is <= the lower 95% quartile of Ilde-cons_tree_BA


acsa <- data.frame(post[,,"r_species[Acsa,plot_cons_tree_BA]"])
acsa <- c(t(acsa))
qnorm(p = 0.025, mean = mean(acsa), sd = sd(acsa))

acsa_het <- data.frame(post[,,"r_species[Acsa,plot_het_tree_BA]"])
acsa_het <- c(t(acsa_het))
pnorm(q = 0.02904842, mean = mean(acsa_het), sd = sd(acsa_het))
# there is a 28.6% chance that acsa-het_tree_BA is <= the lower 95% quartile of acsa-cons_tree_BA


cade <- data.frame(post[,,"r_species[Cade,plot_cons_tree_BA]"])
cade <- c(t(cade))
qnorm(p = 0.025, mean = mean(cade), sd = sd(cade))

cade_het <- data.frame(post[,,"r_species[Cade,plot_het_tree_BA]"])
cade_het <- c(t(cade_het))
pnorm(q = -0.04829355, mean = mean(cade_het), sd = sd(cade_het))
# there is a 21.7% chance that cade-het_tree_BA is <= the lower 95% quartile of cade-cons_tree_BA


juvi <- data.frame(post[,,"r_species[Juvi,plot_cons_tree_BA]"])
juvi <- c(t(juvi))
qnorm(p = 0.025, mean = mean(juvi), sd = sd(juvi))

juvi_het <- data.frame(post[,,"r_species[Juvi,plot_het_tree_BA]"])
juvi_het <- c(t(juvi_het))
pnorm(q = -0.06756517, mean = mean(juvi_het), sd = sd(juvi_het))
# there is a 14.9% chance that juvi-het_tree_BA is <= the lower 95% quartile of juvi-cons_tree_BA


fagr <- data.frame(post[,,"r_species[Fagr,plot_cons_tree_BA]"])
fagr <- c(t(fagr))
qnorm(p = 0.025, mean = mean(fagr), sd = sd(fagr))

fagr_het <- data.frame(post[,,"r_species[Fagr,plot_het_tree_BA]"])
fagr_het <- c(t(fagr_het))
pnorm(q = 0.3176026, mean = mean(fagr_het), sd = sd(fagr_het))
# there is a 99.7% chance that fagr-het_tree_BA is <= the lower 95% quartile of fagr-cons_tree_BA


ilop <- data.frame(post[,,"r_species[Ilop,plot_cons_tree_BA]"])
ilop <- c(t(ilop))
qnorm(p = 0.025, mean = mean(ilop), sd = sd(ilop))

ilop_het <- data.frame(post[,,"r_species[Ilop,plot_het_tree_BA]"])
ilop_het <- c(t(ilop_het))
pnorm(q = 0.08045157, mean = mean(ilop_het), sd = sd(ilop_het))
# there is a 67.3% chance that ilop-het_tree_BA is <= the lower 95% quartile of ilop-cons_tree_BA


cacr <- data.frame(post[,,"r_species[Cacr,plot_cons_tree_BA]"])
cacr <- c(t(cacr))
qnorm(p = 0.025, mean = mean(cacr), sd = sd(cacr))

cacr_het <- data.frame(post[,,"r_species[Cacr,plot_het_tree_BA]"])
cacr_het <- c(t(cacr_het))
pnorm(q = 0.05592204, mean = mean(cacr_het), sd = sd(cacr_het))
# there is a 50.7% chance that cacr-het_tree_BA is <= the lower 95% quartile of cacr-cons_tree_BA


cofl <- data.frame(post[,,"r_species[Cofl,plot_cons_tree_BA]"])
cofl <- c(t(cofl))
qnorm(p = 0.025, mean = mean(cofl), sd = sd(cofl))

cofl_het <- data.frame(post[,,"r_species[Cofl,plot_het_tree_BA]"])
cofl_het <- c(t(cofl_het))
pnorm(q = 0.01419437, mean = mean(cofl_het), sd = sd(cofl_het))
# there is a 49.3% chance that cofl-het_tree_BA is <= the lower 95% quartile of cofl-cons_tree_BA


hate <- data.frame(post[,,"r_species[Hate,plot_cons_tree_BA]"])
hate <- c(t(hate))
qnorm(p = 0.025, mean = mean(hate), sd = sd(hate))

hate_het <- data.frame(post[,,"r_species[Hate,plot_het_tree_BA]"])
hate_het <- c(t(hate_het))
pnorm(q = 0.1181973, mean = mean(hate_het), sd = sd(hate_het))
# there is a 51.1% chance that hate-het_tree_BA is <= the lower 95% quartile of hate-cons_tree_BA


kala <- data.frame(post[,,"r_species[Kala,plot_cons_tree_BA]"])
kala <- c(t(kala))
qnorm(p = 0.025, mean = mean(kala), sd = sd(kala))

kala_het <- data.frame(post[,,"r_species[Kala,plot_het_tree_BA]"])
kala_het <- c(t(kala_het))
pnorm(q = -0.02932659, mean = mean(kala_het), sd = sd(kala_het))
# there is a 30.2% chance that kala-het_tree_BA is <= the lower 95% quartile of kala-cons_tree_BA


tscana <- data.frame(post[,,"r_species[Tscana,plot_cons_tree_BA]"])
tscana <- c(t(tscana))
qnorm(p = 0.025, mean = mean(tscana), sd = sd(tscana))

tscana_het <- data.frame(post[,,"r_species[Tscana,plot_het_tree_BA]"])
tscana_het <- c(t(tscana_het))
pnorm(q = 0.05818416, mean = mean(tscana_het), sd = sd(tscana_het))
# there is a 50.3% chance that tscana-het_tree_BA is <= the lower 95% quartile of tscana-cons_tree_BA


rhma <- data.frame(post[,,"r_species[Rhma,plot_cons_tree_BA]"])
rhma <- c(t(rhma))
qnorm(p = 0.025, mean = mean(rhma), sd = sd(rhma))

rhma_het <- data.frame(post[,,"r_species[Rhma,plot_het_tree_BA]"])
rhma_het <- c(t(rhma_het))
pnorm(q = -0.05493711, mean = mean(rhma_het), sd = sd(rhma_het))
# there is a 14.4% chance that rhma-het_tree_BA is <= the lower 95% quartile of rhma-cons_tree_BA


acpe <- data.frame(post[,,"r_species[Acpe,plot_cons_tree_BA]"])
acpe <- c(t(acpe))
qnorm(p = 0.025, mean = mean(acpe), sd = sd(acpe))

acpe_het <- data.frame(post[,,"r_species[Acpe,plot_het_tree_BA]"])
acpe_het <- c(t(acpe_het))
pnorm(q = -0.1127198, mean = mean(acpe_het), sd = sd(acpe_het))
# there is a 5.5% chance that acpe-het_tree_BA is <= the lower 95% quartile of acpe-cons_tree_BA


ulal <- data.frame(post[,,"r_species[Ulal,plot_cons_tree_BA]"])
ulal <- c(t(ulal))
qnorm(p = 0.025, mean = mean(ulal), sd = sd(ulal))

ulal_het <- data.frame(post[,,"r_species[Ulal,plot_het_tree_BA]"])
ulal_het <- c(t(ulal_het))
pnorm(q = -0.04196768, mean = mean(ulal_het), sd = sd(ulal_het))
# there is a 11.1% chance that ulal-het_tree_BA is <= the lower 95% quartile of ulal-cons_tree_BA


qumo <- data.frame(post[,,"r_species[Qumo,plot_cons_tree_BA]"])
qumo <- c(t(qumo))
qnorm(p = 0.025, mean = mean(qumo), sd = sd(qumo))

qumo_het <- data.frame(post[,,"r_species[Qumo,plot_het_tree_BA]"])
qumo_het <- c(t(qumo_het))
pnorm(q = -0.4086263, mean = mean(qumo_het), sd = sd(qumo_het))
# there is a 33.1% chance that qumo-het_tree_BA is <= the lower 95% quartile of qumo-cons_tree_BA


acfl <- data.frame(post[,,"r_species[Acfl,plot_cons_tree_BA]"])
acfl <- c(t(acfl))
qnorm(p = 0.025, mean = mean(acfl), sd = sd(acfl))

acfl_het <- data.frame(post[,,"r_species[Acfl,plot_het_tree_BA]"])
acfl_het <- c(t(acfl_het))
pnorm(q = -0.2548341, mean = mean(acfl_het), sd = sd(acfl_het))
# there is a 3.1% chance that acfl-het_tree_BA is <= the lower 95% quartile of acfl-cons_tree_BA



# NEGATIVE
prse_het <- data.frame(post[,,"r_species[Prse,plot_het_tree_BA]"])
prse_het <- c(t(prse_het))
qnorm(p = 0.025, mean = mean(prse_het), sd = sd(prse_het))

prse <- data.frame(post[,,"r_species[Prse,plot_cons_tree_BA]"])
prse <- c(t(prse))
pnorm(q = -0.298064, mean = mean(prse), sd = sd(prse))
# there is a 79.4% chance that prse-con_tree_BA is <= the lower 95% quartile of prse-het_tree_BA


quru_het <- data.frame(post[,,"r_species[Quru,plot_het_tree_BA]"])
quru_het <- c(t(quru_het))
qnorm(p = 0.025, mean = mean(quru_het), sd = sd(quru_het))

quru <- data.frame(post[,,"r_species[Quru,plot_cons_tree_BA]"])
quru <- c(t(quru))
pnorm(q = -0.2921097, mean = mean(quru), sd = sd(quru))
# there is a 95.9% chance that quru-con_tree_BA is <= the lower 95% quartile of quru-het_tree_BA


litu_het <- data.frame(post[,,"r_species[Litu,plot_het_tree_BA]"])
litu_het <- c(t(litu_het))
qnorm(p = 0.025, mean = mean(litu_het), sd = sd(litu_het))

litu <- data.frame(post[,,"r_species[Litu,plot_cons_tree_BA]"])
litu <- c(t(litu))
pnorm(q = -0.407361, mean = mean(litu), sd = sd(litu))
# there is a 95.4% chance that litu-con_tree_BA is <= the lower 95% quartile of litu-het_tree_BA


list_het <- data.frame(post[,,"r_species[List,plot_het_tree_BA]"])
list_het <- c(t(list_het))
qnorm(p = 0.025, mean = mean(list_het), sd = sd(list_het))

list <- data.frame(post[,,"r_species[List,plot_cons_tree_BA]"])
list <- c(t(list))
pnorm(q = -0.1866789, mean = mean(list), sd = sd(list))
# there is a 99.8% chance that list-con_tree_BA is <= the lower 95% quartile of list-het_tree_BA


caov_het <- data.frame(post[,,"r_species[Caov,plot_het_tree_BA]"])
caov_het <- c(t(caov_het))
qnorm(p = 0.025, mean = mean(caov_het), sd = sd(caov_het))

caov <- data.frame(post[,,"r_species[Caov,plot_cons_tree_BA]"])
caov <- c(t(caov))
pnorm(q = -0.3307749, mean = mean(caov), sd = sd(caov))
# there is a 88.1% chance that caov-con_tree_BA is <= the lower 95% quartile of caov-het_tree_BA


rops_het <- data.frame(post[,,"r_species[Rops,plot_het_tree_BA]"])
rops_het <- c(t(rops_het))
qnorm(p = 0.025, mean = mean(rops_het), sd = sd(rops_het))

rops <- data.frame(post[,,"r_species[Rops,plot_cons_tree_BA]"])
rops <- c(t(rops))
pnorm(q = -0.2703336, mean = mean(rops), sd = sd(rops))
# there is a 93.5% chance that rops-con_tree_BA is <= the lower 95% quartile of rops-het_tree_BA


quve_het <- data.frame(post[,,"r_species[Quve,plot_het_tree_BA]"])
quve_het <- c(t(quve_het))
qnorm(p = 0.025, mean = mean(quve_het), sd = sd(quve_het))

quve <- data.frame(post[,,"r_species[Quve,plot_cons_tree_BA]"])
quve <- c(t(quve))
pnorm(q = -0.3912916, mean = mean(quve), sd = sd(quve))
# there is a 99.6% chance that quve-con_tree_BA is <= the lower 95% quartile of quve-het_tree_BA


pita_het <- data.frame(post[,,"r_species[Pita,plot_het_tree_BA]"])
pita_het <- c(t(pita_het))
qnorm(p = 0.025, mean = mean(pita_het), sd = sd(pita_het))

pita <- data.frame(post[,,"r_species[Pita,plot_cons_tree_BA]"])
pita <- c(t(pita))
pnorm(q = -0.6126871, mean = mean(pita), sd = sd(pita))
# there is a 92.6% chance that pita-con_tree_BA is <= the lower 95% quartile of pita-het_tree_BA


frpe_het <- data.frame(post[,,"r_species[Frpe,plot_het_tree_BA]"])
frpe_het <- c(t(frpe_het))
qnorm(p = 0.025, mean = mean(frpe_het), sd = sd(frpe_het))

frpe <- data.frame(post[,,"r_species[Frpe,plot_cons_tree_BA]"])
frpe <- c(t(frpe))
pnorm(q = -0.1641409, mean = mean(frpe), sd = sd(frpe))
# there is a 60.1% chance that frpe-con_tree_BA is <= the lower 95% quartile of frpe-het_tree_BA


saal_het <- data.frame(post[,,"r_species[Saal,plot_het_tree_BA]"])
saal_het <- c(t(saal_het))
qnorm(p = 0.025, mean = mean(saal_het), sd = sd(saal_het))

saal <- data.frame(post[,,"r_species[Saal,plot_cons_tree_BA]"])
saal <- c(t(saal))
pnorm(q = -0.2499484, mean = mean(saal), sd = sd(saal))
# there is a 64.8% chance that saal-con_tree_BA is <= the lower 95% quartile of saal-het_tree_BA


bele_het <- data.frame(post[,,"r_species[Bele,plot_het_tree_BA]"])
bele_het <- c(t(bele_het))
qnorm(p = 0.025, mean = mean(bele_het), sd = sd(bele_het))

bele <- data.frame(post[,,"r_species[Bele,plot_cons_tree_BA]"])
bele <- c(t(bele))
pnorm(q = -0.3383095, mean = mean(bele), sd = sd(bele))
# there is a 63.2% chance that bele-con_tree_BA is <= the lower 95% quartile of bele-het_tree_BA


cato_het <- data.frame(post[,,"r_species[Cato,plot_het_tree_BA]"])
cato_het <- c(t(cato_het))
qnorm(p = 0.025, mean = mean(cato_het), sd = sd(cato_het))

cato <- data.frame(post[,,"r_species[Cato,plot_cons_tree_BA]"])
cato <- c(t(cato))
pnorm(q = -0.3583573, mean = mean(cato), sd = sd(cato))
# there is a 53.6% chance that cato-con_tree_BA is <= the lower 95% quartile of cato-het_tree_BA



# INTRASPECIFIC INTERACTIVE EFFECTS
# PC1
# POSITIVE
litu <- data.frame(post[,,"r_species[Litu,plot_cons_tree_BA:pc1]"])
litu <- c(t(litu))
qnorm(p = 0.025, mean = mean(litu), sd = sd(litu))

litu_het <- data.frame(post[,,"r_species[Litu,plot_het_tree_BA:pc1]"])
litu_het <- c(t(litu_het))
pnorm(q = -0.1157361, mean = mean(litu_het), sd = sd(litu_het))
# there is a 85.2% chance that litu-het_tree_BA is <= the lower 95% quartile of litu-cons_tree_BA


list <- data.frame(post[,,"r_species[List,plot_cons_tree_BA:pc1]"])
list <- c(t(list))
qnorm(p = 0.025, mean = mean(list), sd = sd(list))

list_het <- data.frame(post[,,"r_species[List,plot_het_tree_BA:pc1]"])
list_het <- c(t(list_het))
pnorm(q = -0.08351911, mean = mean(list_het), sd = sd(list_het))
# there is a 76.4% chance that list-het_tree_BA is <= the lower 95% quartile of list-cons_tree_BA


nysy <- data.frame(post[,,"r_species[Nysy,plot_cons_tree_BA:pc1]"])
nysy <- c(t(nysy))
qnorm(p = 0.025, mean = mean(nysy), sd = sd(nysy))

nysy_het <- data.frame(post[,,"r_species[Nysy,plot_het_tree_BA:pc1]"])
nysy_het <- c(t(nysy_het))
pnorm(q = -0.087347, mean = mean(nysy_het), sd = sd(nysy_het))
# there is a 71.4% chance that nysy-het_tree_BA is <= the lower 95% quartile of nysy-cons_tree_BA


# NEGATIVE
ulru_het <- data.frame(post[,,"r_species[Ulru,plot_het_tree_BA:pc1]"])
ulru_het <- c(t(ulru_het))
qnorm(p = 0.025, mean = mean(ulru_het), sd = sd(ulru_het))

ulru <- data.frame(post[,,"r_species[Ulru,plot_cons_tree_BA:pc1]"])
ulru <- c(t(ulru))
pnorm(q = -0.1305415, mean = mean(ulru), sd = sd(ulru))
# there is a 43.9% chance that ulru-con_tree_BA is <= the lower 95% quartile of ulru-het_tree_BA



# TEMP
# POSITIVE
cagl <- data.frame(post[,,"r_species[Cagl,plot_cons_tree_BA:tmax]"])
cagl <- c(t(cagl))
qnorm(p = 0.025, mean = mean(cagl), sd = sd(cagl))

cagl_het <- data.frame(post[,,"r_species[Cagl,plot_het_tree_BA:tmax]"])
cagl_het <- c(t(cagl_het))
pnorm(q = -0.2013234, mean = mean(cagl_het), sd = sd(cagl_het))
# there is a 23.5% chance that cagl-het_tree_BA is <= the lower 95% quartile of cagl-cons_tree_BA


# NEGATIVE
litu_het <- data.frame(post[,,"r_species[Litu,plot_het_tree_BA:tmax]"])
litu_het <- c(t(litu_het))
qnorm(p = 0.025, mean = mean(litu_het), sd = sd(litu_het))

litu <- data.frame(post[,,"r_species[Litu,plot_cons_tree_BA:tmax]"])
litu <- c(t(litu))
pnorm(q = -0.2173483, mean = mean(litu), sd = sd(litu))
# there is a 71.0% chance that litu-con_tree_BA is <= the lower 95% quartile of litu-het_tree_BA


qual_het <- data.frame(post[,,"r_species[Qual,plot_het_tree_BA:tmax]"])
qual_het <- c(t(qual_het))
qnorm(p = 0.025, mean = mean(qual_het), sd = sd(qual_het))

qual <- data.frame(post[,,"r_species[Qual,plot_cons_tree_BA:tmax]"])
qual <- c(t(qual))
pnorm(q = -0.1272308, mean = mean(qual), sd = sd(qual))
# there is a 75.6% chance that qual-con_tree_BA is <= the lower 95% quartile of qual-het_tree_BA


quru_het <- data.frame(post[,,"r_species[Quru,plot_het_tree_BA:tmax]"])
quru_het <- c(t(quru_het))
qnorm(p = 0.025, mean = mean(quru_het), sd = sd(quru_het))

quru <- data.frame(post[,,"r_species[Quru,plot_cons_tree_BA:tmax]"])
quru <- c(t(quru))
pnorm(q = -0.09201779, mean = mean(quru), sd = sd(quru))
# there is a 48.5% chance that quru-con_tree_BA is <= the lower 95% quartile of quru-het_tree_BA


# PC2
# POSITIVE
litu <- data.frame(post[,,"r_species[Litu,plot_cons_tree_BA:pc2]"])
litu <- c(t(litu))
qnorm(p = 0.025, mean = mean(litu), sd = sd(litu))

litu_het <- data.frame(post[,,"r_species[Litu,plot_het_tree_BA:pc2]"])
litu_het <- c(t(litu_het))
pnorm(q = -0.1435046, mean = mean(litu_het), sd = sd(litu_het))
# there is a 83.5% chance that litu-het_tree_BA is <= the lower 95% quartile of litu-cons_tree_BA


