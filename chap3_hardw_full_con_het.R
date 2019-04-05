
setwd("/pine/scr/a/l/alissab/Chap3")

require(dplyr)
require(brms)
require(future)
require(mice)

# MAKE SURE TO CHANGE THE NAME OF THE DATASET YOU'RE USING FOR ALL LONGLEAF .R FILES
# SINCE YOU'VE REMOVED OUTLIERS
# MIGHT WANT TO RE-RUN PCA BEFORE DOING THIS
# THEN RE-MAKE MAPS 
# RE-RUN ANALYSES, MAKING SURE TO CALL 'LOO' IN THE LONGLEAF FILE, OTHERWISE IT
# WON'T WORK

dat <- read.csv("chap3_hardw_plots_outl_rem.csv", stringsAsFactors = FALSE, na.strings=c("","NA"), fileEncoding="latin1")
dat <- dat %>% filter(species_name != "other")
dat$plot_het_tree_BA <- with(dat, plot_tree_BA - plot_cons_tree_BA)

# remove infrequent species
species_num <- dat %>% group_by(species) %>% summarise(n_plots = n())
dat <- left_join(dat, species_num, by="species")
dat <- dat %>% filter(n_plots >= 100)

# scale numeric data
scaled_vars <- c("tmax", "mean_prec", "twi", "pc1", "pc2",
                 "plot_het_tree_BA", "plot_cons_tree_BA")
dat[, scaled_vars] <- scale(dat[, scaled_vars])

# impute missing values using mice package
# only select columns you're using in the model
# dat_imp <- dat %>% select(species, sap_plot_count, plot_het_tree_BA, plot_cons_tree_BA,
#                           tmax, mean_prec, twi, pc1, pc2)
# dat_imp <- mice(dat_imp, m = 5, print = FALSE)

# weakly informative priors
prior <- c(set_prior("normal(0, 1)", class = "b", coef= "tmax"),
           set_prior("normal(0, 1)", class = "b", coef= "mean_prec"),
           set_prior("normal(0, 1)", class = "b", coef= "twi"),
           set_prior("normal(0, 1)", class = "b", coef= "pc1"),
           set_prior("normal(0, 1)", class = "b", coef= "pc2"),
           set_prior("normal(0, 1)", class = "b", coef= "plot_het_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "plot_cons_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "tmax:plot_het_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "tmax:plot_cons_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "mean_prec:plot_het_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "mean_prec:plot_cons_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "twi:plot_het_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "twi:plot_cons_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "pc1:plot_het_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "pc1:plot_cons_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "pc2:plot_het_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "pc2:plot_cons_tree_BA"),
           set_prior("student_t(3, 1, 10)", class = "Intercept"),  # default
           set_prior("gamma(0.01, 0.01)", class = "shape"),  # default
           set_prior("student_t(3, 0, 10)", class = "sd"),  # default
           set_prior("normal(1, 5)", class = "sd", group = "species"),
           set_prior("normal(3, 10)", class = "sd", coef = "Intercept", group = "species"), 
           set_prior("normal(5, 5)", class = "sd", coef = "plot_cons_tree_BA", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_het_tree_BA", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_cons_tree_BA:tmax", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_het_tree_BA:tmax", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_cons_tree_BA:mean_prec", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_het_tree_BA:mean_prec", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_cons_tree_BA:twi", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_het_tree_BA:twi", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_cons_tree_BA:pc1", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_het_tree_BA:pc1", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_cons_tree_BA:pc2", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_het_tree_BA:pc2", group = "species")
)

plan(cluster)
mod <- brm( sap_plot_count ~
              tmax + mean_prec + twi + pc1 + pc2 +                         # pop'n-level main effects
              plot_het_tree_BA + plot_cons_tree_BA +
              
              (1 + plot_het_tree_BA + plot_cons_tree_BA +                  # group-level main and interactive effects
                 plot_het_tree_BA:tmax + plot_cons_tree_BA:tmax +
                 plot_het_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec +
                 plot_het_tree_BA:twi + plot_cons_tree_BA:twi + 
                 plot_het_tree_BA:pc1 + plot_cons_tree_BA:pc1 + 
                 plot_het_tree_BA:pc2 + plot_cons_tree_BA:pc2 | species) +
              
              plot_het_tree_BA:tmax + plot_cons_tree_BA:tmax +            # pop'n-level interactions
              plot_het_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec +
              plot_het_tree_BA:twi + plot_cons_tree_BA:twi + 
              plot_het_tree_BA:pc1 + plot_cons_tree_BA:pc1 + 
              plot_het_tree_BA:pc2 + plot_cons_tree_BA:pc2,
            
            family = negbinomial(link = "log", link_shape = "log"),  
            future = TRUE,  
            save_all_pars = TRUE,  
            control = list(adapt_delta = 0.9, stepsize = 0.01, max_treedepth = 15),
            prior = prior,
            init_r = 0.1,
            # warmup = 1000,
            # iter = 2000,
            # chains = 4,
            data = dat
)

save.image("chap3_hardw_full_con_het.RData")

