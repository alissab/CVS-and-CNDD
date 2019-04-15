
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
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 1, color = "red", alpha = 0.4) +
  ggtitle("Effect of conspecific basal area\n ") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))



ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-2.1, 1.5)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of heterospecific basal area\n ") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14))




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

sap <- data.frame(post[, , "b_Intercept"])
sap <- c(t(sap))  # if you want a vector of all the pulls from the posterior
mcmc_areas(post, pars = "b_Intercept", prob = 0.8, prob_outer = 0.95, point_est = "mean")
pnbinom(q = 1.5, size = 0.55, mu = mean(sap))  # probabilities you want to report
# q = quantile you're interested in; size = shape parameter; mu = mean of posterior

cons <- data.frame(post[, , "b_plot_cons_tree_BA"])
cons <- c(t(cons))
mcmc_areas(post, pars = "b_plot_cons_tree_BA", prob = 0.8, prob_outer = 0.95, point_est = "mean")
pnorm(q = 0.1, mean = mean(cons), sd = sd(cons))
# mean = mean of posterior, sd = sd of posterior
# 90% probability that cons BA is at or below 0.1

twi_BA <- data.frame(post[,,"b_twi:plot_cons_tree_BA"])
twi_BA <- c(t(twi_BA))
mcmc_areas(post, pars = "b_twi:plot_cons_tree_BA", prob = 0.8, prob_outer = 0.95, point_est = "mean")
qnorm(p = 0.025, mean = mean(twi_BA), sd = sd(twi_BA))
# 95% lower quartile: 0.049464

twi_het <- data.frame(post[,,"b_twi:plot_het_tree_BA"])
twi_het <- c(t(twi_het))
mcmc_areas(post, pars = "b_twi:plot_het_tree_BA", prob = 0.8, prob_outer = 0.95, point_est = "mean")
pnorm(q = 0.049464, mean = mean(twi_het), sd = sd(twi_het))
# there is a 61.8% chance that TWI:het_tree_BA is equal to or less than 
# the lower 95% quartile of TWI:cons_tree_BA

