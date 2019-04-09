
require(rgdal)
require(maps)
require(dplyr)
require(ggplot2)
require(sjPlot)
require(sjstats)
require(gridExtra)


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




# PLOTTING PARAMETER ESTIMATES

# BELOW CODE CHUNK DIDN'T WORK; INSTEAD OF USING SJPLOT, USING GGPLOT2
# plot(marginal_effects(hardw_full_con_het, effects = "plot_cons_tree_BA:pc1"))
# plot_model(hardw_full_con_het, type = "pred", terms = c("plot_cons_tree_BA:pc1")) + 
#   theme_classic()
# 
# grid.arrange(plot_model(hardw_full_con_het, type = "pred", terms = c("plot_het_tree_BA")),
#              plot_model(hardw_full_con_het, type = "pred", terms = c("plot_cons_tree_BA"))) + 
#   theme_classic()



# PLOT RANDOM EFFECTS
# species:con vs. species:het
# hardw_full_con_het$ranef$coef
# plot_model(hardw_full_con_het, type = "re", ri.nr = ) + theme_classic()

ran <- ranef(hardw_full_con_het)
ran$species[,,1]  # extracts random intercept values for each species[1:3]
ran$species[,,2]  # plot_het_tree_BA
ran$species[,,3]  # plot_cons_tree_BA

# ran$species[species names, column estimates, random slope]
ran$species[1:3, , ]  # extracts all random slopes for each species[1:3]

require(sjstats)
cons_betas <- as.data.frame(ran$species[,, 3])  # random intercepts of conspBA for all species
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
  scale_y_continuous(name ="", limits = c(-3.5, 4.5)) +
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
    axis.text.y = element_text(size = 12))



ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-3.5, 4.5)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
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
  scale_y_continuous(name ="", limits = c(-1, 1)) +
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
    axis.text.y = element_text(size = 12))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-1, 1)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and temperature") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))







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
  scale_y_continuous(name ="", limits = c(-0.75, 0.75)) +
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
    axis.text.y = element_text(size = 12))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.75, 0.75)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and precipitation") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))





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
  scale_y_continuous(name ="", limits = c(-0.8, 0.8)) +
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
    axis.text.y = element_text(size = 12))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-0.8, 0.8)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and wetness") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))




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
  scale_y_continuous(name ="", limits = c(-1.2, 1.1)) +
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
    axis.text.y = element_text(size = 12))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-1.2, 1.1)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and soil PCA axis 1") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))




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
  scale_y_continuous(name ="", limits = c(-1.8, 1.5)) +
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
    axis.text.y = element_text(size = 12))


# heterospecific random slope by species
ggplot(con_het_betas, aes(x = reorder(term, conEstimate), y = hetEstimate)) +
  scale_y_continuous(name ="", limits = c(-1.8, 1.5)) +
  scale_x_discrete(name ="") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 1, color = "blue") + 
  geom_errorbar(aes(ymax = hetQ97.5, ymin = hetQ2.5), width = 0, size = 1, color = "blue", alpha = 0.4) +
  ggtitle("Effect of interaction between \nheterospecific basal area and soil PCA axis 2") +
  theme_classic() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 12))





# PLOTTING FIXED EFFECTS
plot(marginal_effects(hardw_full_con_het, effects = "plot_cons_tree_BA:twi", 
                      points = TRUE, rug = TRUE))

 
 


# BELOW CODE IS A MESS AND DOESN'T WORK WELL 
require(tidybayes)
fixed <- as.data.frame(fixef(hardw_full_con_het))
fixed$term <- rownames(fixed)


get_variables(hardw_full_con_het)[1:18]

require(modelr)
dat %>%
  data_grid(plot_cons_tree_BA = seq_range(plot_cons_tree_BA, n = 101),
            plot_het_tree_BA = mean(plot_het_tree_BA, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            mean_prec = mean(mean_prec, na.rm = TRUE), 
            twi = mean(twi, na.rm = TRUE), 
            pc1 = mean(pc1, na.rm = TRUE),
            pc2 = mean(pc2, na.rm = TRUE),
            species = rep("Acru", 101)) %>%
  add_fitted_draws(hardw_full_con_het, value = "sap_plot_count", n = 100) %>%
  
ggplot(data = test, aes(x = plot_cons_tree_BA, y = sap_plot_count)) +
  # geom_point(data = dat, aes(x = plot_cons_tree_BA, y = sap_plot_count), alpha = 0.1) +
  stat_lineribbon(.width = 0.95) + 
  scale_fill_brewer() +
  theme_classic()

newdat <- data_grid(data = dat, plot_cons_tree_BA = seq_range(plot_cons_tree_BA, n = 100),
                    plot_het_tree_BA = mean(plot_het_tree_BA, na.rm = TRUE),
                    tmax = mean(tmax, na.rm = TRUE),
                    mean_prec = mean(mean_prec, na.rm = TRUE), 
                    twi = mean(twi, na.rm = TRUE), 
                    pc1 = mean(pc1, na.rm = TRUE),
                    pc2 = mean(pc2, na.rm = TRUE),
                    species = rep("Acru", 100))

test <- fitted(object = hardw_full_con_het, 
               newdata = newdat, 
               resp = sap_plot_count)



# EXTRA CODE
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
