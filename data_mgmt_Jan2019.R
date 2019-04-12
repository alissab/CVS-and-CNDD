
require(dplyr)
require(car)
require(aspace)
require(vegan)
require(tidyr)
require(aspace)


plot <- read.csv("CVS_plots.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
stem <- read.csv("CVS_stems.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
soil <- read.csv("CVS_soils_avg.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
nvc <- read.csv("CVS_comm_classification.csv",stringsAsFactors = FALSE, na.strings=c("","NA"))
VA <- read.csv("VA_CVS_plots_to_use.csv",stringsAsFactors = FALSE, na.strings=c("","NA"))

# manage NVC codes (National Vegetation Classification)
nvc_names <- nvc %>% select(DivisionCode, AssocElcode, AssocSci.name, AssocCom.name)

# select only veg types that are forests or woodlands
nvc_use <- nvc_names[substring(nvc_names$DivisionCode, 1, 1)=="1",]

# remove NAs and duplicated rows
nvc_use <- nvc_use %>% filter(!is.na(AssocElcode))
nvc_use <- nvc_use %>% filter(!is.na(AssocSci.name))
nvc_use <- nvc_use %>% filter(!is.na(AssocCom.name))
nvc_use <- nvc_use[!duplicated(nvc_use),]           # 364 obs

# most CEGL codes follow standard 10-digit format. There are some cells with no CEGLs, one
# with an obscure CEGL format (typo?) and a dozen or so with "CEGL.NoneExists"
# CEGL codes in nvc_use dataframe should be same format as those in comm dataframe

# manage plot data
# fix levels of state
plot$state <- recode(plot$state, "'Alabama'='AL'; 'Arkansas'='AR'; 'NORTH CAROLINA'='NC';
'North Carolina'='NC'; 'SOUTH CAROLINA'='SC'; 'South Carolina'='SC'; 'FLORIDA'='FL';
'Florida'='FL'; 'GEORGIA'='GA'; 'Georgia'='GA'; 'Maryland'='MD'; 'Mississippi'='MS';
'TENNESSEE'='TN'; 'Tennessee'='TN'; 'TEXAS'='TX'; 'Texas'='TX'; 'Virginia'='VA';
'West Virginia'='WV'")

## turn observation start date into year
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
plot$year <- substrRight(plot$Observation_Start_Date, 4)
plot$year <- as.numeric(plot$year)  # one start date == "####", so will be coerced to NA value
plot <- plot %>% select(Author_Observation_Code, Observation_Start_Date, province, state, 
                        countyName, Public_Latitude, Public_Longitude, realUTME, realUTMN, 
                        realUTMZone, Canopy_Height_in_meters, Coordinate_System, Coordinate_Units, 
                        Longitude_or_UTM.E, Latitude_or_UTM.N, UTM_Zone, Geocoordinate_Datum, 
                        Author_Location, Elevation, Elevation_Accuracy, Aspect, Slope, Topographic_Position, 
                        Method_Narrative, commPrimaryScientific, commPrimaryCode, commPrimaryCommon, 
                        previousObsCode, year)
colnames(plot)[1] <- "plot"


# there are 2299 rows that do not have CEGL codes. retain for now.
# remove plots not located within woodlands or forests using nvc_use dataframe
# retain CEGLs of "NA"
keep <- nvc_use$AssocElcode
plot <- plot %>% filter(is.na(commPrimaryCode) | commPrimaryCode %in% keep)

# remove plots in WV (they only measured stems >= 7cm DBH)
plot <- plot %>% filter(state != "WV")

# remove most of VA plots (those that did not measure woody stems using same 
# protocol as rest of CVS data - received these plots from Karen Patterson at VA NHP)
# first, need to fix naming convention
# plot name conversion (from VA naming system to CVS naming system)

# remove plot duplicates/resamples
remove <- c("D","R")  # should only be 2 such plots
fix <- VA[nchar(VA$Plot) == 8,]
fix <- data.frame(fix[,1, drop = FALSE])
fix <- data.frame(fix[!substr(fix$Plot, 8, 8) %in% remove, , drop = FALSE])

# combine 7-digit plot codes with 8-digit plot codes
fix2 <- data.frame(VA[nchar(VA$Plot) == 7, 1, drop = FALSE])
plot_names <- rbind(fix, fix2)

# convert to CVS naming system
colnames(plot_names)[1] <- "old_name"
plot_names$alpha <- substr(plot_names$old_name,1,4)
plot_names$num <- substr(plot_names$old_name,5,nchar(plot_names$old_name))
# plot_names$cvs_plot <- paste0(substr(plot_names$cvs_name,1,10),
#                               substr(plot_names$cvs_name,12,nchar(plot_names$cvs_name)))

plot_names$cvs_plot <- paste0("085-", plot_names$alpha, "-0", plot_names$num)

# Find plots in full dataset that were collected by VA NHP; remove those whose plot names
# are not within plot_names$cvs_plot
va_plot <- plot[grepl("085-", plot$plot),]
plot <- plot[!grepl("085-", plot$plot),]
va_keep <- va_plot[va_plot$plot %in% plot_names$cvs_plot,]
plot <- rbind(plot, va_keep)


# soil data downloaded from CVS database as plot-averaged, horizon-averaged (???) values
colnames(soil) <- c("organic","sand","silt","clay","coarse","ph","exchCap","baseSat","N","S","P",
                  "Ca_ppm","Mg_ppm","K_ppm","Na_ppm","Ca_pct","Mg_pct","K_pct","Na_pct","Other_pct",
                  "H_pct","B_ppm","Fe_ppm","Mn_ppm","Cu_ppm","Zn_ppm","Al_ppm","density",
                  "Ca_Mg_ppm","obs_ID","Plot")
soil <- soil %>% select(-obs_ID)

# clean stem data

# module names are inconsistent
stem$Mod <- recode(stem$Mod,"'mod 1 '='mod 1';'mod 2 '='mod 2';'mod 3 '='mod 3';
                     'mod 4 '='mod 4';'mod 8 '='mod 8';'mod 9 '='mod 9'; 'mod r'='mod R'")

# remove rows in Stem that have no plot or module size info
# (I think these rows might be for individual (large) trees whose canopies are in the plot, 
# but the base is not.)
stem <- stem %>% filter(!is.na(ModArea) | !is.na(moduleArea) | !is.na(PlotArea))

# Bob found error in how certain rows were aggregated - remove stems for which the diameter
# is recorded as 39.37cm
# Also remove NA diameters (only 16 of these)
stem <- stem[!is.na(stem$Diameter),]
stem <- stem[stem$Diameter!=39.37,]

# fix species names; lots of inconsistencies
colnames(stem)[2] <- "SpeciesName"
stem$SpeciesName <- recode(stem$SpeciesName,"'ACERRUB'='Acer rubrum'; 'Betula lenta var. lenta'='Betula lenta'; 'CARYCOR'='Carya cordiformis'; 'CARYGLA'='Carya glabra'; 'CARYOVT'='Carya ovata'; 'DIOSVIR'='Diospyros virginiana'; 'FAGUGRA'='Fagus grandifolia'; 'FRAXAME'='Fraxinus americana'; 'Fraxinus [americana + biltmoreana + smallii]'='Fraxinus americana'; 'Hamamelis virginiana var. virginiana'='Hamamelis virginiana'; 'ILEXAMB'='Ilex ambigua'; 'ILEXDEC'='Ilex decidua'; 'Ilex decidua var. decidua'='Ilex decidua'; 'ILEXOPAO'='Ilex opaca'; 'ILEXVER'='Ilex verticillata'; 'Ilex verticillata sp.'='Ilex verticillata'; 'JUNIVIR'='Juniperus virginiana'; 'LIQUSTY'='Liquidambar styraciflua'; 'LIRITUL'='Liriodendron tulipifera'; 'Magnolia virginiana var. virginiana'='Magnolia virginiana'; 'MORURUBR'='Morus rubra'; 'Morus rubra sp.'='Morus rubra'; 'NYSSSYL'='Nyssa sylvatica'; 'OXYDARB'='Oxydendrum arboreum'; 'PINUECH'='Pinus echinata'; 'PINUPAL'='Pinus palustris'; 'PINUTAE'='Pinus taeda'; 'PLATOCC'='Platanus occidentalis'; 'PRUNSER'='Prunus serotina'; 'Prunus serotina var. serotina'='Prunus serotina'; 'QUERFAL'='Quercus falcata'; 'QUERMRL'='Quercus marilandica'; 'QUERNIG'='Quercus nigra'; 'QUERPHE'='Quercus phellos'; 'QUERRUB'='Quercus rubra'; 'Quercus rubra var. rubra'='Quercus rubra'; 'QUERSTE'='Quercus stellata'; 'QUERVEL'='Quercus velutina'; 'SASSALB'='Sassafras albidum'; 'Acer negundo var. negundo'='Acer negundo'; 'Tilia americana var. heterophylla'='Tilia americana'; 'Tilia americana var. americana'='Tilia americana';'Tilia americana var. caroliniana'='Tilia americana';'ULMUALA'='Ulmus alata'; 'ULMURUB'='Ulmus rubra'; 'VACCARBA'='Vaccinium arboreum'; 'VACCCOR'='Vaccinium corymbosum'; 'Vaccinium corymbosum sp. #2'='Vaccinium corymbosum'; 'Liriodendron tulipifera var. tulipifera'='Liriodendron tulipifera'; 'Juniperus virginiana var. virginiana'='Juniperus virginiana'; 'Ilex opaca var. opaca'='Ilex opaca'; 'Fagus grandifolia var. caroliniana'='Fagus grandifolia'; 'Cercis canadensis var. canadensis'='Cercis canadensis'; 'Carpinus caroliniana var. caroliniana'='Carpinus caroliniana'; 'Acer rubrum var. rubrum'='Acer rubrum'; 'Toxicodendron radicans var. radicans'='Toxicodendron radicans'; 'Muscadinia rotundifolia var. rotundifolia'='Muscadinia rotundifolia'; 'Prunus alleghaniensis var. alleghaniensis'='Prunus alleghaniensis'; 'Prunus umbellata var. umbellata'='Prunus umbellata';'Quercus marilandica var. marilandica'='Quercus marilandica';'Viburnum dentatum var. dentatum'='Viburnum dentatum';'Carpinus caroliniana ssp. caroliniana'='Carpinus caroliniana'")

# table of species with plot count (this is the sampling unit)
# RAN INTO PROBLEMS because there are plot/mod/sps/size duplicates.
# EMAILED BOB about issue. Not clear what root of problem is, so will just have to 
# remove duplicates. (I.e., it's not clear whether they are actual data duplicates, or whether 
# they should be added together). There are 24,124 such rows.
stem <- stem[!(duplicated(stem[,1:4]) | duplicated(stem[,1:4], fromLast = TRUE)), ]

# some provinces are left blank, but you can fill them in manually
counties <- c("Gaston", "Lincoln", "Durham", "Orange", "Wake", "Granville", "Chatham")
plot$province[plot$countyName %in% counties] <- "Piedmont"
plot$province[plot$countyName=="Watauga"] <- "Mountains"

prov <- left_join(stem[, c(1,2)], plot[, c(1,3,5)], by=c("Plot" = "plot"))



# import USDA plants data to get growth habit info for each species;
# only analyzing trees/shrubs
# downloaded from website: https://plants.sc.egov.usda.gov/dl_all.html
species <- stem %>% distinct(SpeciesName)
usda <- read.csv("USDA_plants_growth_habit.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
keep <- left_join(species, usda[, c("Scientific.Name", "Growth.Habit", "Native.Status")], 
                  by = c("SpeciesName" = "Scientific.Name"))
keep <- keep[!is.na(keep$Growth.Habit), ]
keep <- keep %>% separate(Growth.Habit, c("gh1", "gh2", "gh3"), sep = ",")

keep$habit <- with(keep, ifelse(
  gh1 == "Tree", "trees", ifelse(
      gh1 == "Shrub", "shrubs", "remove"
    )
  )
)
keep <- keep %>% filter(gh1 ==  "Tree" | gh1 == "Shrub")

# remove vines and subshrubs
keep <- keep %>% filter(gh2 != "Vine")
keep <- keep %>% filter(gh2 != "Subshrub")

# in a separate project, I manually extracted growth habits for certain species via USDA plants website;
# add these to dataframe.

# (old code)
# remove species that aren't shrubs or trees (or that are nonnative)
# categorize species as shrub, shrub-tree, or tree
# (categorized based on USDA plants website "growth habit")
remove <- c('Arundinaria gigantea','Arundinaria tecta', 'Berchemia scandens', 'Bignonia capreolata','Campsis radicans', 'Euonymus americanus', 'Gelsemium sempervirens', 'Ligustrum sinense','Lonicera japonica','Muscadinia rotundifolia','Parthenocissus quinquefolia','Smilax bona-nox [var. bona-nox + var. littoralis]','Smilax bona-nox','Smilax glauca','Smilax laurifolia','Smilax rotundifolia','Toxicodendron radicans', 'Vitis aestivalis', 'Vitis vulpina','Arundinaria appalachiana','Ditrysinia fruticosa','Erythrina herbacea','Hypericum fasciculatum','Hypericum myrtifolium','Hypericum suffruticosum','Ligustrum japonicum','Ligustrum lucidum','Melia azedarach','Nekemias arborea','Pieris phillyreifolia','Sabal minor','Serenoa repens','Smilax auriculata','Smilax smallii','Smilax walteri','Yucca aloifolia')
shrubs <- c('Clethra alnifolia','Gaylussacia ursina','Hydrangea arborescens','Ilex glabra','Leucothoe fontanesiana','Lyonia lucida','Rhododendron [carolinianum + minus]','Rhododendron calendulaceum','Vaccinium corymbosum', 'Vaccinium formosum', 'Vaccinium fuscatum', 'Vaccinium stamineum', 'Viburnum rafinesqueanum')
shrub.trees <- c('Acer pensylvanicum','Acer saccharum','Aesculus flava','Aesculus sylvatica','Alnus serrulata','Amelanchier arborea','Amelanchier laevis','Asimina triloba','Carpinus caroliniana','Celtis laevigata','Cercis canadensis','Chionanthus virginicus','Cyrilla racemiflora','Cornus florida','Fraxinus caroliniana','Hamamelis virginiana','Ilex coriacea','Ilex decidua','Ilex montana','Ilex opaca', 'Ilex verticillata', 'Ilex vomitoria','Kalmia latifolia','Lindera benzoin','Magnolia virginiana','Morella cerifera','Ostrya virginiana','Oxydendrum arboreum','Persea palustris','Prunus serotina', 'Quercus incana', 'Rhododendron catawbiense','Rhododendron maximum','Sassafras albidum','Symplocos tinctoria','Vaccinium arboreum','Viburnum prunifolium')
trees <- c('Acer floridanum', 'Acer negundo', 'Acer rubrum','Betula alleghaniensis','Betula lenta','Betula nigra','Carya cordiformis','Carya glabra','Carya ovalis','Carya ovata','Carya tomentosa','Castanea dentata','Diospyros virginiana','Fagus grandifolia','Fraxinus americana','Fraxinus pennsylvanica','Halesia tetraptera','Juglans nigra','Juniperus virginiana','Liquidambar styraciflua','Liriodendron tulipifera','Magnolia acuminata','Magnolia fraseri','Morus rubra','Nyssa aquatica','Nyssa biflora','Nyssa sylvatica','Picea rubens','Pinus echinata','Pinus palustris', 'Pinus pungens', 'Pinus rigida','Pinus serotina','Pinus strobus','Pinus taeda','Pinus virginiana','Platanus occidentalis','Quercus alba','Quercus coccinea','Quercus falcata','Quercus laevis', 'Quercus lyrata', 'Quercus laurifolia', 'Quercus marilandica var. marilandica', 'Quercus michauxii','Quercus montana', 'Quercus muehlenbergii', 'Quercus nigra', 'Quercus pagoda', 'Quercus phellos','Quercus rubra','Quercus stellata','Quercus velutina', 'Quercus virginiana', 'Robinia pseudoacacia','Taxodium ascendens','Taxodium distichum','Tilia americana','Tsuga canadensis','Ulmus alata','Ulmus americana','Ulmus rubra')
keep_stems <- as.data.frame(c(shrubs, shrub.trees, trees))
names(keep_stems) <- "SpeciesName"
keep_stems$SpeciesName <- as.character(keep_stems$SpeciesName)

# combine new species vector with old species vector and remove duplicates
all_keep_stems <- c(keep$SpeciesName, keep_stems$SpeciesName)
all_keep_stems <- unique(all_keep_stems)  # 193

# remove species in "remove" vector
all_keep_stems <- all_keep_stems[!all_keep_stems %in% remove]  # 172

# species in "all_keep_stems" vector will be analyzed as the response (sapling count); 
# others will be kept to calculate BAs and counts, but won't be analyzed as response;
# (look through list of species to remove to make sure you aren't dumping important species.
# the assumption is that they aren't relatively common trees/shrubs if they aren't in 
# "all_keep_stems", and thus not appropriate for this analysis)

# create "other" group for species not being analyzed individually
stem$species <- ifelse(stem$SpeciesName %in% all_keep_stems, stem$SpeciesName, "other")

# for certain plots, stems were measured differently than CVS protocol. find those cases and remove them
cvs_class <- c(0.5,1.75,3.75,7.5,12.5,17.5,22.5,27.5,32.5,37.5)
cvs <- stem[stem$Diameter>=40 | stem$Diameter %in% cvs_class,]


#' summary of "active" dataframes so far: 
#' cvs - stem counts/measurements; stems IDed to species level (common sps) or as 'other' (rarer sps)
#' soil - soil measurements; averaged across horizons/plot
#' plot - plot information, including community classification codes (CEGLs)

# since we're comparing 'sapling' counts to 'adult' basal area, need to define these two size classes.
# separate species dataframe into sap/tree size classes
sap <- cvs %>% filter(Diameter<=3.75)
tree <- cvs %>% filter(Diameter>3.75)
sap <- sap %>% select(-Diameter)   # can remove Diameter from sap
colnames(sap)[4] <- "sap_count"

# for trees, calculate basal area: pi * (DBH/2)^2
tree$BA <- pi*((tree$Diameter)/2)^2
tree$BA2 <- tree$StemCount * tree$BA
tree <- tree %>% select(-c(Diameter, StemCount, BA))
colnames(tree)[9] <- "tree_BA"


## calculate sum of counts/BAs for plots/modules/species with more than one row of counts
sap <- sap %>% group_by(Plot, Mod, species) %>% summarise(sap_count = sum(sap_count))
tree <- tree %>% group_by(Plot, Mod, species) %>% summarise(tree_BA = sum(tree_BA))


## merge the Small and Large dataframes
stem <- full_join(sap, tree, by = c("Plot","Mod", "species"))
stem[is.na(stem)] <- 0			## change NAs to zeros

# "stem" is now the active stem dataframe (not "cvs")

# add module/plot sizes back into dataframe (from "cvs" to "stem")
plot_size <- cvs %>% select(Plot, Mod, ModArea, subsampling_factor, moduleArea, PlotArea)
plot_size <- distinct(plot_size)

# futher edits needed based on plot size
# remove rows where module size is larger than plot size - likely data entry errors
plot_size <- plot_size %>% mutate(Diff = PlotArea-ModArea, diff = PlotArea-moduleArea)
plot_size <- plot_size %>% filter(is.na(Diff) | Diff>=0)
plot_size <- plot_size %>% filter(is.na(diff) | diff>=0)
plot_size <- plot_size %>% select(-Diff, -diff)

# remove rows where subsampling factor is > 1 (this indicates that stems outside 
# plot boundaries were surveyed.)
plot_size <- plot_size %>% filter(subsampling_factor <= 1)

# ModArea is the size of the area surveyed within the module, and moduleArea is the size
# of the module. If a module was subsampled, ModArea will be smaller than moduleArea. 
# Rows for which this is not the case should be removed (data entry error?)
plot_size <- plot_size %>% mutate(same = ifelse(subsampling_factor < 1, "subsamp",
                                          ifelse(subsampling_factor == 1 & is.na(ModArea), NA,
                                            ifelse(subsampling_factor == 1 & is.na(moduleArea), NA,
                                              ifelse(ModArea==moduleArea, "same", "diff")))))
plot_size <- plot_size %>% filter(is.na(same) | same != "diff")

# might want to remove modules that were subsampled b/c of coding issues later on...
# kept finding duplicated rows with very large #s/BAs, but not sure why
plot_size <- plot_size %>% filter(same != "subsamp")
plot_size <- plot_size %>% select(-same)

# merge plot size info back into stem dataframe
# all.x=FALSE means that rows with plot size data entry errors won't show up in "stem" df
stem2 <- merge(stem, plot_size, by=c("Plot","Mod"), all.x=FALSE, all.y=TRUE)

# merge soil with stem data
dat <- merge(stem2, soil, by="Plot", all.x=TRUE, all.y=FALSE)

# merge dat with plot info
# not keeping all 'dat' rows b/c before we filtered out plots for various reasons, and 
# don't want those plots in the dataframe
dat <- merge(dat, plot, by.x="Plot", by.y="plot", all.x=FALSE, all.y=FALSE)

# I THINK THIS CODE FOR ACCOUNTING FOR SUBSAMPLED MODULES IS CAUSING ISSUES...
# correct basal areas and sapling counts by dividing by subsampling factors
#dat <- dat %>% mutate(c_sap_count = sap_count/subsampling_factor, 
#                      c_tree_BA = tree_BA/subsampling_factor)
#dat <- dat[,c(1:3, 59, 60, 4:58)]  # just easier to see sap, tree counts/BAs
#dat <- dat %>% select(-ModArea, -subsampling_factor)

# c_sap and c_tree values that are Inf or NA can be removed (likely typo; only 39 of these)
#dat2 <- dat %>% filter(!is.infinite(c_sap_count), !is.infinite(c_tree_BA), 
#                        !is.na(c_sap_count), !is.na(c_tree_BA))

# Won't analyze residual plot data, so can remove those
dat2 <- dat %>% filter(Mod != "mod R")

# remove modules where module size (moduleArea) is <100m2 or "NA"
# this will make comparison across modules less complicated
dat2 <- dat2 %>% filter(!is.na(moduleArea) & moduleArea == 100)

# need to remove mods that aren't contiguous
# also remove plots that don't include the four contiguous 
# traditional intensive modules (mods 2, 3, 8, 9)
keep <- c("mod 2", "mod 3", "mod 8", "mod 9")
dat2 <- dat2 %>% filter(Mod %in% keep)


# sampling unit is each combination of Plot/Mod/Species, so we already know:
# conspecific sapling count within the module; and
# conspecific adult BA within the module.
# Need to calculate: 
# total sapling count within module;
# total adult BA across plot;
# total conspecific adult BA across plot

# aggregate total sapling count within each module
total_sap <- dat2 %>% select(Plot, Mod, sap_count)
total_sap <- total_sap %>% group_by(Plot, Mod) %>% summarise(tot_mod_sap_count = 
                                                               sum(sap_count))
# add back to main df
dat2 <- left_join(dat2, total_sap, by = c("Plot", "Mod"))

# aggregate tree_BA across all modules within the same plot
total_BA <- dat2 %>% select(Plot, tree_BA)
total_BA <- total_BA %>% group_by(Plot) %>% summarise(plot_tree_BA = sum(tree_BA))
# add back to main df
dat2 <- left_join(dat2, total_BA, by = "Plot")

# aggregate tree_BA across modules within same plot by species to get
# conspecific tree BA
cons_BA <- dat2 %>% select(Plot, species, tree_BA)
cons_BA <- cons_BA %>% group_by(Plot, species) %>% summarise(plot_cons_tree_BA = 
                                                                      sum(tree_BA))

# add back to main df
dat2 <- left_join(dat2, cons_BA, by = c("Plot", "species"))

# calculate proportion of adult BA made up by conspecifics
prop_cons <- dat2 %>% mutate(prop_cons = plot_cons_tree_BA / plot_tree_BA)


# make slope/aspect conversions
# 3 terms for 2 vars; allows model to fit this independently for each sps
# slope/aspect need to be transformed to radians first 

# convert to radians 
dat2$slopeR <- as_radians(dat2$Slope)
dat2$aspectR <- as_radians(dat2$Aspect)

# create new topography variables (pers. comm. with J. Clark)
dat2$top1 <- sin(dat2$slopeR)
dat2$top2 <- sin(dat2$slopeR) * sin(dat2$aspectR)
dat2$top3 <- sin(dat2$slopeR) * cos(dat2$aspectR)


# reduce length of species names by giving them four-letter codes
# first replace Carya ovalis with Carya glabra (synonyms)
dat3 <- dat2
dat3$species <- ifelse(dat3$species=="Carya ovalis", "Carya glabra", dat3$species)

species <- as.data.frame(dat3$species)
names(species) <- "species_name"
names(dat3)[3] <- "species_name"  # just to make things easier later on
species$species <- paste0(substr(species$species_name,start=1, stop=2), sub("^\\S+\\s+", '', species$species_name))
species$species <- substr(species$species, start=1, stop=4)
species[species == "otot"] <- "other"
species <- unique(species)

#check for identical 4-letter species codes and fix
species <- species %>% arrange(species)
species$species_name <- as.character(species$species_name)
which(duplicated(species$species))

species$species <- with(species, ifelse(
  species_name == "Carpinus caroliniana", "Cacr", ifelse(
    species_name == "Frangula caroliniana", "Frang_ca", ifelse(
      species_name == "Ilex amelanchier", "Ilamel", ifelse(
        species_name == "Ilex ambigua", "Ilambi", ifelse(
          species_name == "Ilex coriacea", "Ilcori", ifelse(
            species_name == "Ilex collina", "Ilcoll", ifelse(
              species_name == "Ilex cornuta", "Ilcorn", ifelse(
                species_name == "Quercus ×bushii", "Quxbush", ifelse(
                  species_name == "Quercus ×blufftonensis", "Quxbluff", ifelse(
                    species_name == "Quercus ×subintegra", "Quxsub", ifelse(
                      species_name == "Quercus ×saulii", "Quxsau", ifelse(
                        species_name == "Quercus laevis", "Qulaev", ifelse(
                          species_name == "Quercus laurifolia", "Qulaur", ifelse(
                            species_name == "Quercus marilandica", "Qumari", ifelse(
                              species_name == "Quercus margaretta", "Qumarg", ifelse(
                                species_name == "Quercus minima", "Qumini", ifelse(
                                  species_name == "Quercus michauxii", "Qumich", ifelse(
                                    species_name == "Rhododendron catawbiense", "Rhcata", ifelse(
                                      species_name == "Rhododendron calendulaceum", "Rhcale", ifelse(
                                        species_name == "Rhododendron carolinianum", "Rhcaro", ifelse(
                                          species_name == "Rhododendron canescens", "Rhcane", ifelse(
                                            species_name == "Tsuga canadensis", "Tscana", ifelse(
                                              species_name == "Tsuga caroliniana", "Tscaro",
                                              species))))))))))))))))))))))))

# add back to main df
dat4 <- left_join(dat3, species, by="species_name")

# add climate data
clim <- read.csv("cvs_climate.csv", stringsAsFactors = FALSE)
va <- read.csv("cvs_va_climate.csv", stringsAsFactors = FALSE)
clim <- rbind(clim, va)
clim <- unique(clim)
dat4 <- merge(dat4, clim, by.x="Plot", by.y="plot", all.x=TRUE, all.y=FALSE)


# need to remove plots for which we don't have ALL four contiguous intensive mods
test <- dat4 %>% select(Plot, Mod) %>% distinct()
test <- test %>% group_by(Plot) %>% summarise(n = n())
table(test$n)  # 2242 plots that contain 4 intensive modules (2,3,8,9)
test <- test %>% filter(n==4)
keep <- test$Plot   # Plots to keep
dat5 <- dat4 %>% filter(Plot %in% keep)

write.csv(dat5, "chap3_data_by_mod_10April.csv", row.names = FALSE)
# this is the final df to use if you want the sampling unit to be each combination of 
# plot/mod/species, where: 
# sap_count = # sap stems of particular species within plot/mod
# tree_BA = BA of all adult stems of a particular species within plot/mod
# tot_mod_sap_count = total # sap stems across each plot/mod
# plot_tree_BA = total adult BA across plot
# plot_cons_tree_BA = adult BA of conspecifics across plot


# create another df that combines all modules, so sampling unit becomes every combination
# of plot/species (suggested by committee in Feb 2019)

# extract mod-specific info that needs to be summarized on the plot scale, then summarize
mod_summ_c_sap <- dat5 %>% select(Plot, species, sap_count)
mod_summ_c_sap <- mod_summ_c_sap %>% group_by(Plot, species) %>% summarise(sap_plot_count = 
                                                                             sum(sap_count))
mod_summ_tot_sap <- dat5 %>% select(Plot, sap_count)
mod_summ_tot_sap <- mod_summ_tot_sap %>% group_by(Plot) %>% summarise(tot_plot_sap_count = 
                                                                        sum(sap_count))

dat6 <- dat5 %>% select(-Mod, -sap_count, -tree_BA, -ModArea, -subsampling_factor, -moduleArea, 
                        -PlotArea, -tot_mod_sap_count)
dat6 <- dat6 %>% distinct()
dat6 <- inner_join(mod_summ_c_sap, dat6, by=c("Plot", "species"))
dat6 <- inner_join(dat6, mod_summ_tot_sap, by=c("Plot"))


# add moisture index info (from Conghe Song)
# TWI = total wetness index = slope/UAA (upslope accumulated area)
moisture <- read.csv("twi_with_NAs.csv", stringsAsFactors = FALSE, na.strings="999999")
moisture <- moisture %>% select(ID, twi)
names(moisture) <- c("Plot", "twi")

dat7 <- left_join(dat6, moisture, by = "Plot")
write.csv(dat7, "chap3_data_by_plot_10April.csv", row.names = FALSE)

# this is the final df to use if you want the sampling unit to be each combination of 
# plot/species, where: 
# sap_plot_count = # sap stems of particular species across plot
# tot_plot_sap_count = total # sap stems across plot
# plot_cons_tree_BA = adult BA of conspecifics across plot
# plot_tree_BA = total adult BA across plot
# But still need to run PCA on soil vars if you want to use this df (un-culled) in analysis


# need to cull dataset to remove inappropriate vegetation types from analysis
# look at veg types in df
veg <- dat7 %>% select(Plot, commPrimaryCommon)
veg <- distinct(veg)
veg <- veg %>% group_by(commPrimaryCommon) %>% summarise(n = n())
veg <- veg %>% filter(!is.na(commPrimaryCommon))

# trick is to remove veg types with sufficiently unique recruitment patterns that those plots
# would mask recruitment pattern of interest

# remove maritime/estuarine fringe/fringe/marsh hammock; highly affected by salt input
# remove savannas/barrens/sandhill scrubs; soil moisture/nutrients sufficiently diff't
# include levee forest/bottomland hardwoods/floodplains, but remove marsh/peatland/bogs/swamps/impoundments; soil sufficiently diff't
# include alluvial forests/headwater stream forests/nonriverine wet hardwood forests/wet marl?
# remove pocosins, such as pond pine woodland/bay forest/pocosin/atlantic white cedar forest/canebreaks
# wet pine flatwoods/sandhill seep/cypress savannas
# remove tidal red cedar forest
veg_substr <- "Fringe|Maritime|Marsh|Peat|Bog|Swamp|Pocosin|Loblolly|Pond-cypress|Sandhill Scrub|Successional|Sandhill|Hammock|Outcrop|Tidal|Bluff"
veg_remove <- veg[grep(veg_substr, veg$commPrimaryCommon), ]
dat_rem <- dat7 %>% filter(!commPrimaryCommon %in% veg_remove$commPrimaryCommon)

# looks OK, but there are a lot of commPrimaryCommon cells with NA, altho they do have CEGLs
# there are fewer NAs using commPrimaryScientific
nas <- dat_rem[is.na(dat_rem$commPrimaryScientific),]
nrow(nas[is.na(nas$commPrimaryCode),])  # 418 NAs

# find veg community types for those that DO have CEGLs
# leave out the 1000 rows that do not have any veg type names/codes?
nas_table <- as.data.frame(nas[!is.na(nas$commPrimaryCode), "commPrimaryCode"])
names(nas_table) <- "commPrimaryCode"
nas_table <- unique(nas_table)   # four CEGLs that you need to add a veg type name for
# CEGL007126, CEGL007127, CEGL007129, CEGL007125

# manually look up veg types associated with those CEGLs
# add new veg types back into dat_rem dataframe
dat_rem$commPrimaryCommon <- ifelse(
  dat_rem$commPrimaryCode == "CEGL007126", "Longleaf Pine - Pond Pine / Turkey Oak / Blue Huckleberry / Little Bluestem Woodland Association", ifelse(
    dat_rem$commPrimaryCode == "CEGL007127", "Longleaf Pine / Turkey Oak / Woody-goldenrod / Arrowfeather Three-awn Woodland Association", ifelse(
      dat_rem$commPrimaryCode == "CEGL007129", "Longleaf Pine / Sand Post Oak / Atlantic Poison-oak / Little Bluestem Woodland Association", ifelse(
        dat_rem$commPrimaryCode == "CEGL007125", "Longleaf Pine / Turkey Oak - Sand Live Oak / Little Bluestem Woodland Association", 
        dat_rem$commPrimaryCommon
      )
    )
  )
)

dat_rem$commPrimaryScientific <- ifelse(
  dat_rem$commPrimaryCode == "CEGL007126", "Pinus palustris - Pinus serotina / Quercus laevis / Gaylussacia frondosa / Schizachyrium scoparium Woodland Association", ifelse(
    dat_rem$commPrimaryCode == "CEGL007127", "Pinus palustris / Quercus laevis / Chrysoma pauciflosculosa / Aristida purpurascens Woodland Association", ifelse(
      dat_rem$commPrimaryCode == "CEGL007129", "Pinus palustris / Quercus margarettiae / Toxicodendron pubescens / Schizachyrium scoparium Woodland Association", ifelse(
        dat_rem$commPrimaryCode == "CEGL007125", "Pinus palustris / Quercus laevis - Quercus geminata / Schizachyrium scoparium Woodland Association", 
        dat_rem$commPrimaryScientific
      )
    )
  )
)

# start culling data using scientific names
# remove "Ruderal" and "Sabal palmetto" plots
remove2 <- dat_rem[grep("Ruderal|Sabal palmetto ", dat_rem$commPrimaryScientific), ]
dat_rem <- dat_rem[!dat_rem$Plot %in% remove2$Plot, ]

# this second culling iteration leaves you with all the plots you decided to keep before, 
# plus those plots you added in manually using CEGL codes
veg2 <- dat_rem %>% select(Plot, commPrimaryScientific)
veg2 <- distinct(veg2)
veg2 <- veg2 %>% group_by(commPrimaryScientific) %>% summarise(n = n())
veg2 <- veg2 %>% filter(!is.na(commPrimaryScientific))

# extreme environments/ruderal plots have already been removed, so now you just want 
# to separate mixed hardwood from pine plots
pine_substr <- "Abies fraseri /|Pinus palustris |Picea rubens - |Picea rubens - |Pinus echinata - |Pinus echinata / Schizachyrium|Pinus palustris - |Pinus palustris / | / Juniperus virginiana|Pinus elliottii var. elliottii |Pinus rigida |Pinus serotina|Pinus strobus|Pinus taeda - Chamaecyparis|Pinus taeda - Quercus|Pinus virginiana |Taxodium distichum - Fraxinus|Taxodium distichum - Liquidambar|Taxodium distichum - Taxodium|Tsuga canadensis / Kalmia|Tsuga canadensis / Rhododendron maximum - |Tsuga canadensis - Halesia|Tsuga canadensis - Liriodendron|Tsuga caroliniana |Juniperus virginiana var. virginiana - |Juniperus virginiana var. silicicola - "
pine_plots <- veg2[grep(pine_substr, veg2$commPrimaryScientific), ]
hardw_plots <- veg2[!veg2$commPrimaryScientific %in% pine_plots$commPrimaryScientific, ]

pine_dat <- dat_rem %>% filter(commPrimaryScientific %in% pine_plots$commPrimaryScientific)  # 3274 rows
hardwood_dat <- dat_rem %>% filter(commPrimaryScientific %in% hardw_plots$commPrimaryScientific) # 16282 rows

# df UPDATE
# pine_dat: culled, pine data
# hardw_dat: culled, hardwood data
# dat_rem: culled, all data
# dat7: unculled, all data


# there are extreme outliers in twi, plot_cons_tree_BA, and plot_tree_BA that 
# should be removed. for twi, these very wet plots are probably not the veg type
# you're wanting to analyze. for others, outlier removal will help with model 
# convergence

dat8 <- dat7 %>% filter(twi<=1500 & plot_cons_tree_BA<=16000 & plot_tree_BA<=60000 & 
                          sap_plot_count <= 130)
write.csv(dat8, "chap3_data_by_plot_outl_rem.csv", row.names = FALSE)

dat_culled <- dat_rem %>% filter(twi<=1500 & plot_cons_tree_BA<=16000 & plot_tree_BA<=60000 & 
                                   sap_plot_count <= 130)
write.csv(dat_culled, "chap3_data_by_plot_culled_outl_rem.csv", row.names = FALSE)

dat_hard <- hardwood_dat %>% filter(twi<=1500 & plot_cons_tree_BA<=16000 & plot_tree_BA<=60000 & 
                                   sap_plot_count <= 130)
write.csv(dat_hard, "chap3_hardw_plots_outl_rem.csv", row.names = FALSE)

dat_pine <- pine_dat %>% filter(twi<=1500 & plot_cons_tree_BA<=16000 & plot_tree_BA<=60000 & 
                                  sap_plot_count <= 130)
write.csv(dat_pine, "chap3_pine_plots_outl_rem.csv", row.names = FALSE)



# after culling df, run PCA on soil measurements
# from column 10 (organic) through column 30 (density)

# ONLY SELECT SOIL VARS THAT ARE RELIABLE MEASURES
# ONLY USE UNIQUE VALUES (ONE SET OF MEASURES PER PLOT)
# REMOVE NAS
# SCALE MEASURES BEFORE RUNNING PCA - CHECK HISTOGRAMS TO MAKE SURE NORMAL

# run PCA on hardwood data only; if you decide to include pine plots, you'll 
# need to re-run PCA; use "chap3_hardw_plots_outl_rem.csv"
soil_use <- dat8 %>% select(Plot, organic:Ca_Mg_ppm)
soil_use <- soil_use %>% select(-N, -S, -P, -coarse)
soil_use <- soil_use %>% distinct
soil_use <- soil_use[complete.cases(soil_use), ]
soil_use[,-1] <- sapply(soil_use[,-1], as.numeric)

par(mfrow=c(6, 5))
par(mar=rep(0.5, 4))
sapply(soil_use[, -1], hist)  # look for data shape

# remove outliers
out_organic <- boxplot(soil_use$organic, plot=FALSE)$out
soil_use[,"organic"] <- ifelse(
  soil_use$organic %in% out_organic, NA, soil_use$organic)

out_exchCap <- boxplot(soil_use$exchCap, plot=FALSE)$out
soil_use[,"exchCap"] <- ifelse(
  soil_use$exchCap %in% out_exchCap, NA, soil_use$exchCap)

out_baseSat <- boxplot(soil_use$baseSat, plot=FALSE)$out
soil_use[,"baseSat"] <- ifelse(
  soil_use$baseSat %in% out_baseSat, NA, soil_use$baseSat)

out_Ca_ppm <- boxplot(soil_use$Ca_ppm, plot=FALSE)$out
soil_use[,"Ca_ppm"] <- ifelse(
  soil_use$Ca_ppm %in% out_Ca_ppm, NA, soil_use$Ca_ppm)

out_Mg_ppm <- boxplot(soil_use$Mg_ppm, plot=FALSE)$out
soil_use[,"Mg_ppm"] <- ifelse(
  soil_use$Mg_ppm %in% out_Mg_ppm, NA, soil_use$Mg_ppm)

out_Na_ppm <- boxplot(soil_use$Na_ppm, plot=FALSE)$out
soil_use[,"Na_ppm"] <- ifelse(
  soil_use$Na_ppm %in% out_Na_ppm, NA, soil_use$Na_ppm)

out_B_ppm <- boxplot(soil_use$B_ppm, plot=FALSE)$out
soil_use[,"B_ppm"] <- ifelse(
  soil_use$B_ppm %in% out_B_ppm, NA, soil_use$B_ppm)

out_Fe_ppm <- boxplot(soil_use$Fe_ppm, plot=FALSE)$out
soil_use[,"Fe_ppm"] <- ifelse(
  soil_use$Fe_ppm %in% out_Fe_ppm, NA, soil_use$Fe_ppm)

out_Mn_ppm <- boxplot(soil_use$Mn_ppm, plot=FALSE)$out
soil_use[,"Mn_ppm"] <- ifelse(
  soil_use$Mn_ppm %in% out_Mn_ppm, NA, soil_use$Mn_ppm)

out_Cu_ppm <- boxplot(soil_use$Cu_ppm, plot=FALSE)$out
soil_use[,"Cu_ppm"] <- ifelse(
  soil_use$Cu_ppm %in% out_Cu_ppm, NA, soil_use$Cu_ppm)

out_Zn_ppm <- boxplot(soil_use$Zn_ppm, plot=FALSE)$out
soil_use[,"Zn_ppm"] <- ifelse(
  soil_use$Zn_ppm %in% out_Zn_ppm, NA, soil_use$Zn_ppm)

out_Mg_pct <- boxplot(soil_use$Mg_pct, plot=FALSE)$out
soil_use[,"Mg_pct"] <- ifelse(
  soil_use$Mg_pct %in% out_Mg_pct, NA, soil_use$Mg_pct)

out_K_pct <- boxplot(soil_use$K_pct, plot=FALSE)$out
soil_use[,"K_pct"] <- ifelse(
  soil_use$K_pct %in% out_K_pct, NA, soil_use$K_pct)

out_Na_pct <- boxplot(soil_use$Na_pct, plot=FALSE)$out
soil_use[,"Na_pct"] <- ifelse(
  soil_use$Na_pct %in% out_Na_pct, NA, soil_use$Na_pct)

out_Other_pct <- boxplot(soil_use$Other_pct, plot=FALSE)$out
soil_use[,"Other_pct"] <- ifelse(
  soil_use$Other_pct %in% out_Other_pct, NA, soil_use$Other_pct)

out_Ca_Mg_ppm <- boxplot(soil_use$Ca_Mg_ppm, plot=FALSE)$out
soil_use[,"Ca_Mg_ppm"] <- ifelse(
  soil_use$Ca_Mg_ppm %in% out_Ca_Mg_ppm, NA, soil_use$Ca_Mg_ppm)

sapply(soil_use[,-1], hist)

soil_scale <- as.data.frame(sapply(soil_use[,-1], scale))
soil_scale <- as.data.frame(c(soil_use[,1], soil_scale))
names(soil_scale)[1] <- "Plot"
sapply(soil_scale[,-1], hist)

# looks good, save scaled, outlier-removed soil data
write.csv(soil_scale, "chap3_soil_hardw_scaled_outlier_rem.csv", row.names = FALSE)


# perform PCA
soil_scale <- soil_scale %>% select(-Plot)
soil_scale <- soil_scale[complete.cases(soil_scale),]
pca <- rda(soil_scale)
par(mfrow=c(1,1))
biplot(pca, display = c("sites", "species"), type = c("text", "points"))

soil_scale$pc1 <- pca$CA$u[,1]
soil_scale$pc2 <- pca$CA$u[,2]
soil_dat <- soil_scale

soil_scale <- read.csv("chap3_soil_hardw_scaled_outlier_rem.csv", stringsAsFactors = FALSE)
soil_scale <- soil_scale[complete.cases(soil_scale),]
soil_dat <- cbind(Plot = soil_scale[,1], soil_dat)

dat_hard <- left_join(dat_hard, soil_dat[ ,c(1, 27, 28)], by = "Plot")
write.csv(dat_hard, "chap3_hardw_plots_USE_10April.csv", row.names = FALSE)




# EXTRA CODE (MAY OR MAY NOT USE)

# after you select plots, determine which species to include in analysis
# based on the extent of their distributions across the provinces
# first combine coastal fringe with coastal plain
dat$province_3 <- ifelse(dat$province == "CoastalFringe", "CoastalPlain", dat$province)

# determine how many plots each species is located on, grouped by province
species <- dat %>% select(Plot, species_name, province_3)
species <- species %>% distinct()
species_nplots <- species %>% group_by(species_name, province_3) %>% summarise(nplots = n())

# add column with number of provinces each species is found within
species_nplots <- species_nplots %>% group_by(species_name) %>% mutate(n_prov = n())
write.csv(species_nplots, "chap3_nplots_by_species-province.csv", row.names = FALSE)


