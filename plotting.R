
require(rgdal)
require(maps)
require(dplyr)
require(ggplot2)
require(ggmap)

require(ggExtra)  # for marginal histograms
require(sjPlot)
require(sjstats)
require(gridExtra)
require(bayesplot)


dat <- read.csv("chap3_hardw_plots_29May.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

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
title(main = "CVS mixed hardwood plot locations, n = 1113 \n \n")


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


png("PC1_mapped.png", width = 7, height = 3.5, units = "in", res = 300)
ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(pc1)), mapping = aes(x = x, y = y, color = pc1), 
             alpha = 0.5, size = 4) + 
  scale_color_gradient(low="lightblue", high="darkblue", name="Soil fertility\n") +
  # geom_point(data = subset(plots, !is.na(pc1)), mapping = aes(x = x, y = y), 
  #            alpha = 0.8, size = 4, pch = 21, color = "black") + 
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),
        plot.margin = unit(rep(0,4), "cm"),
        legend.text = element_text(size = 10))
dev.off()


png("PC2_mapped.png", width = 7, height = 3.5, units = "in", res = 300)
ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(pc2)), mapping = aes(x = x, y = y, color = pc2), 
             alpha = 0.5, size = 4) + 
  scale_color_gradient(low="lightblue", high="darkblue", name="Soil \ncoarseness\n") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        plot.margin = unit(rep(0,4), "cm"))
dev.off()


png("Elevation_mapped.png", width = 7, height = 3.5, units = "in", res = 300)
ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(Elevation)), mapping = aes(x = x, y = y, color = Elevation), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient(low="lightblue", high="darkblue", name="Elevation\n") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),
        plot.margin = unit(rep(0,4), "cm"),
        legend.text = element_text(size = 10))
dev.off()


png("top1_mapped.png", width = 7, height = 3.5, units = "in", res = 300)
ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(top1)), mapping = aes(x = x, y = y, color = top1), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient(low="lightblue", high="darkblue", name="Slope\nsteepness\n") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),
        plot.margin = unit(rep(0,4), "cm"),
        legend.text = element_text(size = 10))
dev.off()




png("top2_mapped.png", width = 7, height = 3.5, units = "in", res = 300)
ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(top2)), mapping = aes(x = x, y = y, color = top2), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient(low="lightblue", high="darkblue", name="East-West\naspect\n") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),
        plot.margin = unit(rep(0,4), "cm"),
        legend.text = element_text(size = 10))
dev.off()




png("top3_mapped.png", width = 7, height = 3.5, units = "in", res = 300)
ggplot() + 
  geom_polygon(data = basemap, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + 
  geom_point(data = subset(plots, !is.na(top3)), mapping = aes(x = x, y = y, color = top3), 
             alpha = 0.8, size = 4) + 
  scale_color_gradient(low="lightblue", high="darkblue", name="North-South\naspect\n") +
  coord_fixed(1.15) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),
        plot.margin = unit(rep(0,4), "cm"),
        legend.text = element_text(size = 10))
dev.off()






# PLOTTING PARAMETER ESTIMATES
# PLOT RANDOM EFFECTS
# species:con vs. species:het
# require(brms)

# don't need to extract mcmc samples if you're just going to 
# plot mean/credible intervals

ran <- ranef(mod)
ran$species[,,1]  # extracts random intercept values for each species
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
png("param_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(cons_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-3, 4)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()

png("param_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-3, 4)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue", alpha = 0.5) +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# ELEVATION is random effect #4, 5 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 5])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 4])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
png("param_ELEV_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-1.1, 1)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# heterospecific random slope by species
png("param_ELEV_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-1, 1)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue") +
  theme_classic() + 
  
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()



# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# PC1 is random effect #6, 7 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 7])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 6])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
png("param_PC1_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-1.5, 1.1)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# heterospecific random slope by species
png("param_PC1_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-1.5, 1.1)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()




# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# PC2 is random effect #8, 9 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 9])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 8])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
png("param_PC2_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.5, 0.6)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# heterospecific random slope by species
png("param_PC2_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.5, 0.6)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()



# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# TOP1 is random effect #10, 11 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 11])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 10])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
png("param_TOP1_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.6, 0.7)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# heterospecific random slope by species
png("param_TOP1_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.6, 0.7)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()



# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# TOP2 is random effect 12, 13 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 13])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 12])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
png("param_TOP2_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.35, 0.3)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# heterospecific random slope by species
png("param_TOP2_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.35, 0.3)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()



# CON VS. HET INTERACTIONS WITH ENVIRONMENT BY SPECIES
# TOP3 is random effect 14, 15 (het, con, respectively)
cons_betas <- as.data.frame(ran$species[,, 15])
cons_betas$term <- row.names(cons_betas)
het_betas <- as.data.frame(ran$species[,, 14])
het_betas$term <- row.names(het_betas)

# combine conspecific with heteros estimates into one df
names(cons_betas) <- c("conEstimate", "conEst.Error", "conQ2.5", "conQ97.5", "term")
names(het_betas) <- c("hetEstimate", "hetEst.Error", "hetQ2.5", "hetQ97.5", "term")
con_het_betas <- full_join(cons_betas, het_betas, by="term")

# conspecific random slope for each species
png("param_TOP3_con_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = conEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.38, 0.29)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "red") + 
  geom_errorbar(aes(ymax = conQ97.5, ymin = conQ2.5), width = 0, size = 0.5, color = "red") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()


# heterospecific random slope by species
png("param_TOP3_het_sps.png", width = 2.5, height = 7.5, units = "in", res = 300)
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.38, 0.29)) +
  scale_x_discrete(name ="") +
  # geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 2, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 0.5, color = "blue") +
  theme_classic() + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))
dev.off()






# PLOTTING FIXED EFFECTS

# plot posterior distributions
require(bayesplot)
post <- mod3$fit@sim$samples[[1]]
post2 <- data.frame(cbind(post$b_plot_tree_BA, post$b_propCon))
names(post2) <- c("b_plot_tree_BA", "b_propCon")

post.plot <- mcmc_areas(post2, pars = c("b_propCon", "b_plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_con_het.png", width = 3, height = 1.5, units = "in", res = 600)
post.plot + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  # coord_cartesian(ylim = c(1.5, 1.7)) +
  scale_y_discrete(labels = c("Conspecific\neffect    ", "Heterospecific\neffect       ")) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank())
dev.off()



post.plot <- mcmc_areas(post, pars = c("b_Elevation:propCon", "b_Elevation:plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_ELEV_con_het.png", width = 1.5, height = 0.8, units = "in", res = 300)
post.plot + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  coord_cartesian(ylim = c(1.5, 2)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
dev.off()




post.plot <- mcmc_areas(post, pars = c("b_pc1:propCon", "b_pc1:plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_PC1_con_het.png", width = 1.5, height = 0.8, units = "in", res = 300)
post.plot + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  coord_cartesian(ylim = c(1.5, 1.9)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
dev.off()




post.plot <- mcmc_areas(post, pars = c("b_pc2:propCon", "b_pc2:plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_PC2_con_het.png", width = 1.5, height = 0.8, units = "in", res = 300)
post.plot + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  coord_cartesian(ylim = c(1.5, 2)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
dev.off()




post.plot <- mcmc_areas(post, pars = c("b_top1:propCon", "b_top1:plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_TOP1_con_het.png", width = 1.5, height = 0.8, units = "in", res = 300)
post.plot + 
  scale_x_continuous(limits = c(-0.12, 0.1), breaks = seq(-0.1, 0.1, 0.1)) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  coord_cartesian(ylim = c(1.5, 2.2)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
dev.off()




post.plot <- mcmc_areas(post, pars = c("b_top2:propCon", "b_top2:plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_TOP2_con_het.png", width = 1.5, height = 0.8, units = "in", res = 300)
post.plot + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  coord_cartesian(ylim = c(1.5, 2.5)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
dev.off()




post.plot <- mcmc_areas(post, pars = c("b_top3:propCon", "b_top3:plot_tree_BA"), 
                        prob = 0.8, prob_outer = 0.95, point_est = "mean")
png("post_TOP3_con_het.png", width = 1.5, height = 0.8, units = "in", res = 300)
post.plot + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 0.75) + 
  coord_cartesian(ylim = c(1.5, 2.4)) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size=10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
dev.off()






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
require(bayesplot)


# use this code if you had to fill in missing data using mice package.
# data are already gathered for you
post <- mod$fit@sim$samples[[1]]


# use this code if you're using one data set (no missing data filling in)
post <- as.array(mod3)  # array: [# iterations, # chains, # parameters]

# if you did above, you need to first
# gather all iterations across all chains together into one column for each 
# predictor of interest, which gives you its posterior distribution
# to do this, use c(t(<df>))


# only the sapling count response needs the neg. binom. distribution; for all
# predictors, you can use normal
sap <- data.frame(post[, , "b_Intercept"])
sap <- c(t(sap))  # if you want a vector of all the pulls from the posterior
mcmc_areas(post, pars = "b_Intercept", prob = 0.8, prob_outer = 0.95, point_est = "mean")
pnbinom(q = 1.5, size = 0.55, mu = mean(sap))  # probabilities you want to report
# q = quantile you're interested in; size = shape parameter; mu = mean of posterior


# GROUP CON VS. HET
tot <- data.frame(post[,,"b_propCon"])
tot <- c(t(tot))
q <- qnorm(p = 0.025, mean = mean(tot), sd = sd(tot))

con <- data.frame(post[,,"b_plot_tree_BA"])
con <- c(t(con))
pnorm(q = q, mean = mean(con), sd = sd(con))
# there is a 98% chance that propCon is >= tot







# AUTOMATE CON VS. HET COMPARISON BY COVARIATE BY SPECIES
post <- as.array(mod3)

# create list of iterations x chains for each parameter (1857 params)
all_post <- list()
for(i in 1:length(post[1, 1, ])){
  all_post[[i]] <- post[, , i]
}

# name each element of the list based on parameter name
params <- post[1, 1, ]
params <- names(params)
names(all_post) <- params

# add all chains together into one column for each parameter separately
fxn <- function(x) {
  c(t(x))
}
all_post2 <- lapply(all_post, fxn)

# only keep params you want to plot or analyze
all_post_red <- all_post2[substr(params, 1, 2) == "b_" | 
                           substr(params, 1, 2) == "r_"]

all_post_red2 <- all_post_red[grepl("propCon", names(all_post_red)) | 
                                      grepl("plot_tree_BA", names(all_post_red))]

# remove overall con/het to make next steps easier
all_post_red2 <- all_post_red2[3:562]

# order params alphabetically
all_post_red2 <- all_post_red2[order(names(all_post_red2))]

# split list up into series of lists grouped by species
all_post_red3 <- split(all_post_red2, substr(names(all_post_red2), 11, 14))

all_post_red4 <- all_post_red3


# ELEVATION
post_elev <- list()
for(i in 1:length(all_post_red4)){
  post_elev[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][8])), 
                              data.frame(het = unlist(all_post_red4[[i]][1])))
}

species_names <- names(all_post_red4)
names(post_elev) <- species_names

elev_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_elev[[i]][["con"]]) < mean(post_elev[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_elev[[i]][["het"]]), sd = sd(post_elev[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_elev[[i]][["con"]]), sd = sd(post_elev[[i]][["con"]]))
    elev_prob[i, "con_lt_het"] <- 1
    elev_prob[i, "prob"] <- p
  }
  if(mean(post_elev[[i]][["con"]]) > mean(post_elev[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_elev[[i]][["con"]]), sd = sd(post_elev[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_elev[[i]][["het"]]), sd = sd(post_elev[[i]][["het"]]))
    elev_prob[i, "con_lt_het"] <- 0
    elev_prob[i, "prob"] <- p
}
}
elev_prob$species <- species_names



# SLOPE STEEPNESS
post_ste <- list()
for(i in 1:length(all_post_red4)){
  post_ste[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][11])), 
                      data.frame(het = unlist(all_post_red4[[i]][4])))
}

names(post_ste) <- species_names

ste_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_ste[[i]][["con"]]) < mean(post_ste[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_ste[[i]][["het"]]), sd = sd(post_ste[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_ste[[i]][["con"]]), sd = sd(post_ste[[i]][["con"]]))
    ste_prob[i, "con_lt_het"] <- 1
    ste_prob[i, "prob"] <- p
  }
  if(mean(post_ste[[i]][["con"]]) > mean(post_ste[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_ste[[i]][["con"]]), sd = sd(post_ste[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_ste[[i]][["het"]]), sd = sd(post_ste[[i]][["het"]]))
    ste_prob[i, "con_lt_het"] <- 0
    ste_prob[i, "prob"] <- p
  }
}
ste_prob$species <- species_names



# EAST-WEST DIRECTION
post_ew <- list()
for(i in 1:length(all_post_red4)){
  post_ew[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][12])), 
                      data.frame(het = unlist(all_post_red4[[i]][5])))
}

names(post_ew) <- species_names

ew_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_ew[[i]][["con"]]) < mean(post_ew[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_ew[[i]][["het"]]), sd = sd(post_ew[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_ew[[i]][["con"]]), sd = sd(post_ew[[i]][["con"]]))
    ew_prob[i, "con_lt_het"] <- 1
    ew_prob[i, "prob"] <- p
  }
  if(mean(post_ew[[i]][["con"]]) > mean(post_ew[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_ew[[i]][["con"]]), sd = sd(post_ew[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_ew[[i]][["het"]]), sd = sd(post_ew[[i]][["het"]]))
    ew_prob[i, "con_lt_het"] <- 0
    ew_prob[i, "prob"] <- p
  }
}
ew_prob$species <- species_names



# NORTH-SOUTH DIRECTION
post_ns <- list()
for(i in 1:length(all_post_red4)){
  post_ns[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][13])), 
                      data.frame(het = unlist(all_post_red4[[i]][6])))
}

names(post_ns) <- species_names

ns_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_ns[[i]][["con"]]) < mean(post_ns[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_ns[[i]][["het"]]), sd = sd(post_ns[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_ns[[i]][["con"]]), sd = sd(post_ns[[i]][["con"]]))
    ns_prob[i, "con_lt_het"] <- 1
    ns_prob[i, "prob"] <- p
  }
  if(mean(post_ns[[i]][["con"]]) > mean(post_ns[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_ns[[i]][["con"]]), sd = sd(post_ns[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_ns[[i]][["het"]]), sd = sd(post_ns[[i]][["het"]]))
    ns_prob[i, "con_lt_het"] <- 0
    ns_prob[i, "prob"] <- p
  }
}
ns_prob$species <- species_names



# FERTILITY
post_fert <- list()
for(i in 1:length(all_post_red4)){
  post_fert[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][9])), 
                      data.frame(het = unlist(all_post_red4[[i]][2])))
}

names(post_fert) <- species_names

fert_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_fert[[i]][["con"]]) < mean(post_fert[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_fert[[i]][["het"]]), sd = sd(post_fert[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_fert[[i]][["con"]]), sd = sd(post_fert[[i]][["con"]]))
    fert_prob[i, "con_lt_het"] <- 1
    fert_prob[i, "prob"] <- p
  }
  if(mean(post_fert[[i]][["con"]]) > mean(post_fert[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_fert[[i]][["con"]]), sd = sd(post_fert[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_fert[[i]][["het"]]), sd = sd(post_fert[[i]][["het"]]))
    fert_prob[i, "con_lt_het"] <- 0
    fert_prob[i, "prob"] <- p
  }
}
fert_prob$species <- species_names



# TEXTURE
post_text <- list()
for(i in 1:length(all_post_red4)){
  post_text[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][10])), 
                      data.frame(het = unlist(all_post_red4[[i]][3])))
}

names(post_text) <- species_names

text_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_text[[i]][["con"]]) < mean(post_text[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_text[[i]][["het"]]), sd = sd(post_text[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_text[[i]][["con"]]), sd = sd(post_text[[i]][["con"]]))
    text_prob[i, "con_lt_het"] <- 1
    text_prob[i, "prob"] <- p
  }
  if(mean(post_text[[i]][["con"]]) > mean(post_text[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_text[[i]][["con"]]), sd = sd(post_text[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_text[[i]][["het"]]), sd = sd(post_text[[i]][["het"]]))
    text_prob[i, "con_lt_het"] <- 0
    text_prob[i, "prob"] <- p
  }
}
text_prob$species <- species_names




# MAIN CON VS. HET EFFECT
post_main <- list()
for(i in 1:length(all_post_red4)){
  post_main[[i]] <- c(data.frame(con = unlist(all_post_red4[[i]][14])), 
                      data.frame(het = unlist(all_post_red4[[i]][7])))
}

names(post_main) <- species_names

main_prob <- data.frame(species = character(), con_lt_het = integer(), prob = numeric())
for(i in 1:40){
  if(mean(post_main[[i]][["con"]]) < mean(post_main[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_main[[i]][["het"]]), sd = sd(post_main[[i]][["het"]]))
    p <- pnorm(q = q, mean = mean(post_main[[i]][["con"]]), sd = sd(post_main[[i]][["con"]]))
    main_prob[i, "con_lt_het"] <- 1
    main_prob[i, "prob"] <- p
  }
  if(mean(post_main[[i]][["con"]]) > mean(post_main[[i]][["het"]])){
    q <- qnorm(p = 0.025, mean = mean(post_main[[i]][["con"]]), sd = sd(post_main[[i]][["con"]]))
    p <- pnorm(q = q, mean = mean(post_main[[i]][["het"]]), sd = sd(post_main[[i]][["het"]]))
    main_prob[i, "con_lt_het"] <- 0
    main_prob[i, "prob"] <- p
  }
}
main_prob$species <- species_names


results_list <- list(elev_prob, ste_prob, ew_prob, ns_prob, fert_prob, text_prob, main_prob)


for(i in 1:length(results_list)){
  results_list[[i]][, "magnitude"] <- ifelse(
    results_list[[i]][, "prob"] < 0.60, "low", ifelse(
      results_list[[i]][, "prob"] < 0.85, "mod", "high"
    )
  )
}

for(i in 1:length(results_list)){
  results_list[[i]][, "color"] <- ifelse(
    results_list[[i]][, "magnitude"] == "high" & results_list[[i]][, "con_lt_het"] == 1, "red", ifelse(
      results_list[[i]][, "magnitude"] == "high" & results_list[[i]][, "con_lt_het"] == 0, "blue", ifelse(
        results_list[[i]][, "magnitude"] == "mod" & results_list[[i]][, "con_lt_het"] == 1, "pink", ifelse(
          results_list[[i]][, "magnitude"] == "mod" & results_list[[i]][, "con_lt_het"] == 0, "lightblue", NA)
    )
  )
  )
}

names(results_list) <- c("elev", "ste", "ew", "ns", "fert", "text", "main")

results_list[["elev"]][, "term"] <- "elev"
results_list[["ste"]][, "term"] <- "ste"
results_list[["ew"]][, "term"] <- "ew"
results_list[["ns"]][, "term"] <- "ns"
results_list[["fert"]][, "term"] <- "fert"
results_list[["text"]][, "term"] <- "text"
results_list[["main"]][, "term"] <- "main"

# bind each param together into one df (row bind)
summary_all <- rbind(data.frame(results_list[["elev"]]), data.frame(results_list[["ste"]]), data.frame(results_list[["ew"]]),
                     data.frame(results_list[["ns"]]), data.frame(results_list[["fert"]]), data.frame(results_list[["text"]]), 
                     data.frame(results_list[["main"]]))

# provide dummy x-variable, one for each env variable
summary_all$x <- with(summary_all, ifelse(
  term == "main", 1, ifelse(term == "elev", 2, ifelse(term == "ste", 3, ifelse(
    term == "ew", 4, ifelse(term == "ns", 5, ifelse(term == "fert", 6, ifelse(
      term == "text", 7, "oops")
    )))
  )))
)
summary_all$x <- as.numeric(summary_all$x)

# write.csv(summary_all, "chap3_results_summary_for_graphing.csv", row.names = FALSE)
# summary_all <- read.csv("chap3_results_summary_for_graphing.csv", stringsAsFactors = FALSE)

# order species by magnitude of CNDD, so order by term "main"
sp_order <- summary_all[summary_all$term == "main",]
sp_order <- sp_order %>% arrange(desc(con_lt_het), desc(prob))
sp_order$order <- 1:40
summary_all <- left_join(summary_all, sp_order[,c("species", "order")], by = "species")

sp_levels <- data.frame(species = summary_all$species, order = summary_all$order)
sp_levels <- unique(sp_levels)
sp_levels <- sp_levels %>% arrange(desc(order))
sp_levels <- sp_levels$species
sp_levels <- as.character(sp_levels)

png("species_probs_summary.png", width = 6, height = 8, units = "in", res = 600)
ggplot(summary_all, aes(x = x, y = species, color = color)) + 
  geom_hline(mapping = NULL, yintercept = seq(1, 40, 2), colour = 'grey80') +
  geom_point(size = 3.5) +
  scale_x_continuous(name = "", breaks = 1:7,
                     labels = c("Main effect", "Elevation", "Slope   \nsteepness",
                                         "East-West\nfacing  ", "North-South\nfacing   ", "Soil fertility", "Soil texture")) +
  scale_y_discrete(name = "", limits = sp_levels) +
  scale_color_manual(values = c("blue", "lightblue", "pink", "red", "white")) +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12, angle = 60, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
dev.off()





# PLOT EFFECT SIZES BY SPECIES

# RESPONSE OF <SINGLE SPECIES> TO ELEV*TOTAL VS. ELEV*CON
# create new dataframe on which to make predictions for con and totalBA
# for each new species you want to analyze, only need to change 'species = "Xxxx"'
# when creating a new dataframe (for both "tree" and "con" new dataframes)
# you'll also need to change the min/max Elevation values for each species
# QURU (-1, 2); QUVE (-1, 0.5); QUAL (-1.05, 0.5); LITU (-0.8, ); MORU (); HAVI (); SAAS ()

# STOPPED AT MORU BC ESTIMATES ARE REALLY BAD. SAMPLE SIZE ISSUE WITH THE SHRUBBY THINGS
# MAY WANT TO REMOVE AND RE-ANALYZE DATA
# REMOVE THINGS WITH FEW OR NO PLOTS WITH BASAL AREAS >1000
# ALSO SPECIES WHOSE SAPLING COUNT NEVER REACHES 10/PLOT

moru <- dat[dat$species == "Moru", ]
new_tree.elev <- data.frame(Elevation = rep(c(quantile(moru$Elevation, 0.25, na.rm = TRUE), 
                                            quantile(moru$Elevation, 0.75, na.rm = TRUE)) , each = 5), 
                      pc1 = mean(moru$pc1, na.rm = TRUE), pc2 = mean(moru$pc2, na.rm = TRUE), 
                      top1 = mean(moru$top1, na.rm = TRUE), top2 = mean(moru$top2, na.rm = TRUE), 
                      top3 = mean(moru$top3, na.rm = TRUE), 
                      plot_tree_BA = rep(seq(quantile(moru$plot_tree_BA, 0.25, na.rm = TRUE), 
                                             quantile(moru$plot_tree_BA, 0.75, na.rm = TRUE), length.out = 5), times = 2), 
                      propCon = mean(moru$propCon, na.rm = TRUE), species = "Moru")
ptree.elev <- predict(mod3, newdata = new_tree.elev)
ptree.elev <- data.frame(Elevation = new_tree.elev$Elevation, 
                            plot_tree_BA = new_tree.elev$plot_tree_BA, 
                            ptree.elev)
# add raw basal area numbers (for plotting)
# import raw data - only use raw data for getting scaling information for plotting
# must use "dat" (in workspace image for best performing model) for creating new data/making predictions

raw <- read.csv("chap3_hardw_plots_29May.csv", stringsAsFactors = FALSE)
raw$streeBA <- scale(raw$plot_tree_BA)
ptree.elev$streeBA <- attr(raw$streeBA, 'scaled:scale') * ptree.elev$plot_tree_BA + attr(raw$streeBA, 'scaled:center')


new_con.elev <- data.frame(Elevation = rep(c(quantile(moru$Elevation, 0.25, na.rm = TRUE), 
                                             quantile(moru$Elevation, 0.75, na.rm = TRUE)) , each = 5), 
                           pc1 = mean(moru$pc1, na.rm = TRUE), pc2 = mean(moru$pc2, na.rm = TRUE), 
                           top1 = mean(moru$top1, na.rm = TRUE), top2 = mean(moru$top2, na.rm = TRUE), 
                           top3 = mean(moru$top3, na.rm = TRUE), 
                           propCon = rep(seq(quantile(moru$propCon, 0.25, na.rm = TRUE), 
                                                  quantile(moru$propCon, 0.75, na.rm = TRUE), length.out = 5), times = 2), 
                           plot_tree_BA = mean(moru$plot_tree_BA, na.rm = TRUE), species = "Moru")
pcon.elev <- predict(mod3, newdata = new_con.elev)
pcon.elev <- data.frame(Elevation = new_con.elev$Elevation, 
                         propCon = new_con.elev$propCon, 
                         pcon.elev)
raw$propCon <- with(raw, plot_cons_tree_BA / plot_tree_BA)
raw$scon <- scale(raw$propCon)
pcon.elev$scon <- attr(raw$scon, 'scaled:scale') * pcon.elev$propCon + attr(raw$scon, 'scaled:center')


# TOTAL BA FOR HIGH ELEVATIONS
# change name of png file each time you change the species
# need to change ylim of graphs
# QURU (0, 18); QUVE (0, 19); QUAL (0, 15); LITU(0, 19); 

png("total_at_HIGH_ELEV_moru.png", width = 1.5, height = 1, units = "in", res = 600)
ggplot(ptree.elev, aes(x=streeBA, y=Estimate)) +
  # scale_y_continuous(limits=c(0, 19), name="") +
  scale_x_continuous(name = "") +
  geom_ribbon(data=ptree.elev[ptree.elev$Elevation > -0.7, ], inherit.aes=FALSE,
              aes(ymin=Q2.5, ymax=Q97.5, x=streeBA),
              alpha=0.2, linetype=3, size=0.75, color="gray60", fill = "gray70") +
  geom_line(data=ptree.elev[ptree.elev$Elevation > -0.7, ], inherit.aes=FALSE,
              aes(x=streeBA, y=Estimate), size=1, color="gray60") +
  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.text=element_text(size=8),
    axis.title=element_blank(),
    plot.margin = unit(rep(1, 4), "mm"))
dev.off()

# CON FOR HIGH ELEVATIONS
png("con_at_HIGH_ELEV_moru.png", width = 1.5, height = 1, units = "in", res = 600)
ggplot(pcon.elev, aes(x=scon, y=Estimate)) +
  # scale_y_continuous(limits=c(0, 19), name = "") +
  scale_x_continuous(limits=c(-0.01, 1.01), name = "") +
  geom_ribbon(data=pcon.elev[pcon.elev$Elevation > -0.7, ], inherit.aes=FALSE,
              aes(ymin=Q2.5, ymax=Q97.5, x=scon),
              alpha=0, linetype=5, size=0.75, color="gray10", fill="gray70") +
  geom_line(data=pcon.elev[pcon.elev$Elevation > -0.7, ], inherit.aes=FALSE,
            aes(x=scon, y=Estimate), size=1, color="gray10") +
  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.text=element_text(size=8),
    axis.title=element_blank(),
    plot.margin = unit(rep(1, 4), "mm"))
dev.off()


# TOTAL BA FOR LOW ELEVATIONS
png("total_at_LOW_ELEV_litu.png", width = 1.5, height = 1, units = "in", res = 600)
ggplot(ptree.elev, aes(x=streeBA, y=Estimate)) +
  scale_x_continuous(name = "") +
  # scale_y_continuous(limits=c(0, 19), name="") +
  geom_ribbon(data=ptree.elev[ptree.elev$Elevation < -0.7, ], inherit.aes=FALSE,
              aes(ymin=Q2.5, ymax=Q97.5, x=streeBA),
              alpha=0.2, linetype=3, size=0.75, color="gray60", fill = "gray70") +
  geom_line(data=ptree.elev[ptree.elev$Elevation < -0.7, ], inherit.aes=FALSE,
            aes(x=streeBA, y=Estimate), size=1, color="gray60") +
  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.text=element_text(size=8),
    axis.title=element_blank(),
    plot.margin = unit(rep(1, 4), "mm"))
dev.off()

# CON FOR LOW ELEVATIONS
png("con_at_LOW_ELEV_litu.png", width = 1.5, height = 1, units = "in", res = 600)
ggplot(pcon.elev, aes(x=scon, y=Estimate)) +
  scale_x_continuous(limits=c(-0.01, 1.1), name = "") +  
  # scale_y_continuous(limits=c(0, 19), name="") +
  geom_ribbon(data=pcon.elev[pcon.elev$Elevation < -0.7, ], inherit.aes=FALSE,
              aes(ymin=Q2.5, ymax=Q97.5, x=scon),
              alpha=0, linetype=5, size=0.75, color="gray10", fill="gray70") +
  geom_line(data=pcon.elev[pcon.elev$Elevation < -0.7, ], inherit.aes=FALSE,
            aes(x=scon, y=Estimate), size=1, color="gray10") +
  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.text=element_text(size=8),
    axis.title=element_blank(),
    plot.margin = unit(rep(1, 4), "mm"))
dev.off()





# OLD CODE

#ANALYZING SPECIES EFFECTS INDIVIDUALLY

# INTERSPECIFIC CONSPECIFIC DD EFFECTS (POSITIVE AND NEGATIVE)
# POSITIVE
ilde <- data.frame(post[, , "r_species[Ilde,propCon]"])
ilde <- c(t(ilde))
q <- qnorm(p = 0.975, mean = mean(ilde), sd = sd(ilde))

ilde_het <- data.frame(post[, , "r_species[Ilde,plot_tree_BA]"])
ilde_het <- c(t(ilde_het))
pnorm(q = q, mean = mean(ilde_het), sd = sd(ilde_het))



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


