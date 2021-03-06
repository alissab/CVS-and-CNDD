
setwd("/pine/scr/a/l/alissab/new_chap3")

require(dplyr)
require(brms)
require(future)
require(mice)

dat <- read.csv("chap3_hardw_plots_29May.csv", stringsAsFactors = FALSE, na.strings=c("","NA"), fileEncoding="latin1")
dat <- dat %>% filter(species_name != "other")
dat$propCon <- with(dat, plot_cons_tree_BA / plot_tree_BA)

# remove infrequent species
species_num <- dat %>% group_by(species) %>% summarise(n_plots = n())
dat <- left_join(dat, species_num, by="species")
dat <- dat %>% filter(n_plots >= 100)

# remove Castanea and shrubs that don't often reach >5cm DBH or often have multi-stem trunks
# also remove species whose sapling count never reaches 10/plot
to_remove <- c("Cade", "Astr", "Libe", "Ceca", "Havi", "Ilde", "Moru", "Saal", "Qust", "Qumich")
dat <- dat %>% filter(! species %in% to_remove)

# scale numeric data
scaled_vars <- c("Elevation", "pc1", "pc2", "propCon", "plot_tree_BA", "top1", "top2", "top3")
dat[, scaled_vars] <- scale(dat[, scaled_vars])

# impute missing values using mice package
# only select columns you're using in the model
# dat_imp <- dat %>% select(species, sap_plot_count, plot_tree_BA, propCon,
#                           Elevation, pc1, pc2, top1, top2, top3)
# dat_imp <- mice(dat_imp, m = 5, print = FALSE)

# weakly informative priors
prior <- c(set_prior("normal(0, 1)", class = "b", coef= "Elevation"),
           set_prior("normal(0, 1)", class = "b", coef= "pc1"),
           set_prior("normal(0, 1)", class = "b", coef= "pc2"),
           set_prior("normal(0, 1)", class = "b", coef= "top1"),
           set_prior("normal(0, 1)", class = "b", coef= "top2"),
           set_prior("normal(0, 1)", class = "b", coef= "top3"),
           set_prior("normal(0, 1)", class = "b", coef= "plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "propCon"),
           
           set_prior("student_t(3, 1, 10)", class = "Intercept"),  # default
           set_prior("gamma(0.01, 0.01)", class = "shape"),  # default
           set_prior("student_t(3, 0, 10)", class = "sd"),  # default
           set_prior("normal(1, 5)", class = "sd", group = "species"),
           set_prior("normal(3, 10)", class = "sd", coef = "Intercept", group = "species"), 
           set_prior("normal(5, 5)", class = "sd", coef = "propCon", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA", group = "species")
)

plan(cluster)
options(future.globals.maxSize = 1887436800)  # = 1024^2 * # of Mb you need space for

mod1 <- brm( sap_plot_count ~
                        Elevation + pc1 + pc2 + top1 + top2 + top3 +   # pop'n-level main effects
                        plot_tree_BA + propCon +
                        
                        (1 + plot_tree_BA + propCon | species),
                      
                      family = negbinomial(link = "log", link_shape = "log"),  
                      future = TRUE,
                      save_all_pars = TRUE,  
                      # control = list(adapt_delta = 0.9, stepsize = 0.01, max_treedepth = 15),
                      prior = prior,
                      init_r = 0.1,
                      warmup = 2000,
                      iter = 6000,
                      # chains = 4,
                      data = dat
)

save.image("plots_mod1_new2.RData")

# model selection
k_mod1 <- kfold(mod1, K = 10)
save.image("plots_mod1_new2.RData")
