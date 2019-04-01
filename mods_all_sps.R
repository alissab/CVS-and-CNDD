
setwd("/pine/scr/a/l/alissab/Chap3")

require(dplyr)
require(brms)
require(future)

# MAKE SURE YOU'RE USING THE CORRECT DATASET
# LOOKS LIKE YOU'RE ANALYZING BY MODULE, NOT BY PLOT
# IF YOU ANALYZE BY MODULE, YOU HAVE TO USE PLOT AS A GROUPING LEVEL

dat <- read.csv("chap3_hardw_plots.csv", stringsAsFactors = FALSE, na.strings=c("","NA"), fileEncoding="latin1")
dat <- dat %>% filter(species_name != "other")

# remove infrequent species
species_num <- dat %>% group_by(species) %>% summarise(n_plots = n())
dat <- left_join(dat, species_num, by="species")
dat <- dat %>% filter(n_plots >= 100)

# here are the priors that you can specify
# this code chunk manually provides the priors
prior <- c(set_prior("normal(0, 0.1)", class = "b", coef= "tmax"),
            set_prior("normal(0, 0.1)", class = "b", coef= "mean_prec"),
            set_prior("normal(0, 0.1)", class = "b", coef= "moisture"),
            set_prior("normal(0, 0.1)", class = "b", coef= "plot_tree_BA"),
            set_prior("normal(0, 0.1)", class = "b", coef= "plot_cons_tree_BA"),
            set_prior("normal(0, 10)", class = "b", coef= "tmax:plot_tree_BA"),
            set_prior("normal(0, 10)", class = "b", coef= "tmax:plot_cons_tree_BA"),
            set_prior("normal(0, 10)", class = "b", coef= "mean_prec:plot_tree_BA"),
            set_prior("normal(0, 10)", class = "b", coef= "mean_prec:plot_cons_tree_BA"),
            set_prior("normal(0, 10)", class = "b", coef= "moisture:plot_tree_BA"),
            set_prior("normal(0, 10)", class = "b", coef= "moisture:plot_cons_tree_BA"),
            set_prior("student_t(3, 1, 10)", class = "Intercept"),  # default
            set_prior("gamma(0.01, 0.01)", class = "shape"),  # default
            set_prior("student_t(3, 0, 10)", class = "sd"),  # default
            set_prior("normal(1, 10)", class = "sd", group = "species"),
            set_prior("normal(1, 10)", class = "sd", coef = "Intercept", group = "species"))

plan(cluster)  # for parallel computing on Longleaf cluster
mod <- brm(sap_plot_count ~
            tmax + mean_prec + moisture + (1|species) +
            plot_tree_BA + plot_cons_tree_BA +
            plot_tree_BA:species + plot_cons_tree_BA:species +
            plot_tree_BA:tmax + plot_cons_tree_BA:tmax +
            plot_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec +
            plot_tree_BA:moisture + plot_cons_tree_BA:moisture +
            plot_tree_BA:tmax:species + plot_cons_tree_BA:tmax:species +
            plot_tree_BA:mean_prec:species + plot_cons_tree_BA:mean_prec:species +
            plot_tree_BA:moisture:species + plot_cons_tree_BA:moisture:species,
  family = negbinomial(link = "log", link_shape = "log"),  # default link function
  future = TRUE,  # parallel computing, uses one core per chain
  save_all_pars = TRUE,  # I think you need this for plotting posteriors?
  control = list(adapt_delta = 0.9, max_treedepth = 10),  # increase adapt_delta for divergent transitions
  prior = prior,
  data = dat
)

save.image("chap3_hardw_mods_all_spp.RData")

# stancode(mod)  # gives you model code in STAN
# standata(mod)  # gives you model data
# summary(mod, waic = TRUE)  # gives you model summary
# ranef(mod)  # gives you group-level model estimates


# ANOTHER WAY TO SPECIFY PRIORS

# this code chunk gives you a dataframe of (mostly) empty priors that you can fill with
# your own priors. there are only 3 default priors already specified for you
# prior <- get_prior(sap_plot_count ~
#              tmax + mean_prec + moisture + (1|species) +
#              plot_tree_BA + plot_cons_tree_BA +
#              plot_tree_BA:species + plot_cons_tree_BA:species +
#              plot_tree_BA:tmax + plot_cons_tree_BA:tmax +
#              plot_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec +
#              plot_tree_BA:moisture + plot_cons_tree_BA:moisture +
#              plot_tree_BA:tmax:species + plot_cons_tree_BA:tmax:species +
#              plot_tree_BA:mean_prec:species + plot_cons_tree_BA:mean_prec:species +
#              plot_tree_BA:moisture:species + plot_cons_tree_BA:moisture:species,
#            family = negbinomial(link = "log", link_shape = "log"),
#            data = dat)

