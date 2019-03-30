
require(dplyr)

# decide which species to analyze based on the extent of their distributions across the provinces
# first combine coastal fringe with coastal plain
dat$province_3 <- ifelse(dat$province == "CoastalFringe", "CoastalPlain", dat$province)

# determine how many plots each species is located on, grouped by province
species <- dat %>% select(Plot, species_name, province_3)
species <- species %>% distinct()
species_nplots <- species %>% group_by(species_name, province_3) %>% summarise(nplots = n())

# add column with number of provinces each species is found within
species_nplots <- species_nplots %>% group_by(species_name) %>% mutate(n_prov = n())
write.csv(species_nplots, "chap3_nplots_by_species-province.csv", row.names = FALSE)


# BEFORE DECIDING HOW TO HANDLE SAMPLE SIZE, extract only veg community types you want to analyze
# remembering that you'd like environmental variety but not at the expense of interpretability.
# I.e., species in very diff't environments than they're usually found may sway results.
# But if this occurs rarely, it shouldn't be an issue. And again, this is still important information.
# HOWEVER there are more extreme veg types that would follow different rules for recruitment, and this
# may mask patterns you'd like to see. I think this means that if you do remove plots based on veg type,
# you need to be able to explain clearly why you did so (ie, what is it about the veg types you kept
# that makes them special or interesting? can you generalize your results or were you too selective of 
# veg type?)
# E.g., might just keep to upland plots, mixed deciduous hardwood plots, etc.

# look at veg types in df
veg <- dat %>% select(Plot, commPrimaryCommon)
veg <- distinct(veg)
veg <- veg %>% group_by(commPrimaryCommon) %>% summarise(n = n())
veg <- veg %>% filter(!is.na(commPrimaryCommon))

# number of plots per veg type runs from 1-66, median of 5
# regardless, more important question is which ones should we exclude? 
# makes sense to stick with mixed hardwood forests, leaving out pine-dominated stuff
# OR just analyze them separately
# trick is to remove veg types with sufficiently unique recruitment patterns that those plots
# would mask recruitment pattern of interest

# remove maritime/estuarine fringe/fringe/marsh hammock; highly affected by salt input
# remove longleaf savannas/barrens/sandhill scrubs; soil moisture/nutrients sufficiently diff't
# include levee forest/bottomland hardwoods/floodplains, but remove marsh/peatland/bogs/swamps/impoundments; soil sufficiently diff't
# include alluvial forests/headwater stream forests/nonriverine wet hardwood forests/wet marl?
# remove pocosins, such as pond pine woodland/bay forest/pocosin/atlantic white cedar forest/canebreaks
# ??? remove pine savannas/wet pine flatwoods/sandhill seep/cypress savannas
# remove tidal red cedar forest

# DON'T FORGET to re-run PCA on soil vars after removing the above plots
# ask Bob whether to include all soil vars or not before doing so

veg_substr <- "Fringe|Maritime|Marsh|Longleaf|Peat|Bog|Swamp|Pocosin|Loblolly|Pond-cypress|Sandhill Scrub|Table Mountain Pine|Hemlock|Successional|Sandhill|Fraser Fir|Hammock|Outcrop|Tidal|Bluff"
veg_remove <- veg[grep(veg_substr, veg$commPrimaryCommon), ]

dat_rem <- dat %>% filter(!commPrimaryCommon %in% veg_remove$commPrimaryCommon)

# looks OK, but there are a lot of commPimaryCommon cells with NA, altho they do have CEGLs
# ask Bob if he has a spreadsheet that connects the two together




# look at number of sapling stems per species per province
# probably  not so useful
species <- dat %>% select(species_name, sap_plot_count, province_3)
species <- species %>% group_by(species_name, province_3) %>% summarise(sap_n = sum(sap_plot_count))
species <- species[species$sap_n != 0, ]

# list of species with the # of provinces they are found within
species2 <- species %>% group_by(species_name) %>% summarise(n = n())

# list of species found within at least two provinces
species_2 <- species2[species2$n >= 2, ]

# list of species found within all three provinces
species_3 <- species2[species2$n > 2, ]
species_3 <- left_join(species_3, species, by="species_name")  # add # stems per province
species_3_nstems <- species_3 %>% group_by(species_name) %>% summarise(tot_n = sum(sap_n))

# list of species found in exactly 2 provinces
extra <- species_2[!species_2$species_name %in% species_3$species_name, ]

