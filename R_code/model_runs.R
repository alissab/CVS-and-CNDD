
setwd('C:/Users/abrow/Documents/CVS-and_CNDD')
# smaller mods just need 20g; larger mods need more - 50g

require(dplyr)
require(brms)
# require(future)
# require(mice)

dat <- readRDS("hardwood_plot_data.RDS")
dat <- dat %>% filter(species_name != "other")

# rename columns
dat <- dat %>% rename(fert = pc1, text = pc2, steep = top1)

# remove infrequent species
species_num <- dat %>% group_by(species) %>% summarise(n_plots = n())
dat <- left_join(dat, species_num, by="species")
dat <- dat %>% filter(n_plots >= 100)

# create proportion of conspecific variable
dat$propCon <- dat$plot_cons_tree_BA / dat$plot_tree_BA

# scale numeric data
scaled_vars <- c("Elevation", "fert", "text", "propCon", "plot_tree_BA", "steep", "NE")
dat[, scaled_vars] <- scale(dat[, scaled_vars])

# impute missing values using mice package
# only select columns you're using in the model
# dat_imp <- dat %>% select(species, sap_plot_count, plot_tree_BA, propCon,
#                           Elevation, fert, text, steep, top2, top3)
# dat_imp <- mice(dat_imp, m = 5, print = FALSE)



#### MODEL 4 ####
# weakly informative priors
prior <- c(set_prior("normal(0, 1)", class = "b", coef= "Elevation"),
           set_prior("normal(0, 1)", class = "b", coef= "fert"),
           set_prior("normal(0, 1)", class = "b", coef= "text"),
           set_prior("normal(0, 1)", class = "b", coef= "steep"),
           set_prior("normal(0, 1)", class = "b", coef= "NE"),
           set_prior("normal(0, 1)", class = "b", coef= "plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "propCon"),
           
           set_prior("normal(0, 1)", class = "b", coef= "Elevation:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "Elevation:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "fert:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "fert:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "text:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "text:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "steep:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "steep:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "NE:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "NE:propCon"),

           set_prior("student_t(3, 1, 10)", class = "Intercept"),  # default
           set_prior("gamma(0.01, 0.01)", class = "shape"),  # default
           set_prior("student_t(3, 0, 10)", class = "sd"),  # default
           set_prior("normal(1, 5)", class = "sd", group = "species"),
           set_prior("normal(3, 10)", class = "sd", coef = "Intercept", group = "species"), 
           set_prior("normal(5, 5)", class = "sd", coef = "propCon", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:Elevation", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:Elevation", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:fert", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:fert", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:text", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:text", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:steep", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:steep", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:NE", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:NE", group = "species")
)

# future::plan(cluster)
# options(future.globals.maxSize = 1887436800)  # = 1024^2 * # of Mb you need space for

mod4 <- brm( sap_plot_count ~
              Elevation + fert + text + steep + NE +   # pop'n-level main effects
              plot_tree_BA + propCon +
              
              (1 + plot_tree_BA + propCon +                  # group-level main and interactive effects
                 plot_tree_BA:Elevation + propCon:Elevation + 
                 plot_tree_BA:fert + propCon:fert + 
                 plot_tree_BA:text + propCon:text + 
                 plot_tree_BA:steep + propCon:steep + 
                 plot_tree_BA:NE + propCon:NE | species) +
              
              plot_tree_BA:Elevation + propCon:Elevation + 
              plot_tree_BA:fert + propCon:fert + 
              plot_tree_BA:text + propCon:text +
              plot_tree_BA:steep + propCon:steep + 
              plot_tree_BA:NE + propCon:NE,
            
            family = negbinomial(link = "log", link_shape = "log"),  
            # future = TRUE,
            save_all_pars = TRUE,  
            # control = list(adapt_delta = 0.9, stepsize = 0.01, max_treedepth = 15),
            prior = prior,
            init_r = 0.1,
            warmup = 1000,
            iter = 5000,
            chains = 4,
            data = dat
)
saveRDS(mod4, "chap3_mod4_Aug2021.RDS")

# model selection
k_mod4 <- kfold(mod4, K = 10)
saveRDS(k_mod4, "chap3_mod4_kfold_Aug2021.RDS")



#### MODEL 3 ####
# weakly informative priors
prior <- c(set_prior("normal(0, 1)", class = "b", coef= "Elevation"),
           set_prior("normal(0, 1)", class = "b", coef= "fert"),
           set_prior("normal(0, 1)", class = "b", coef= "text"),
           set_prior("normal(0, 1)", class = "b", coef= "steep"),
           set_prior("normal(0, 1)", class = "b", coef= "NE"),
           set_prior("normal(0, 1)", class = "b", coef= "plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "propCon"),

           set_prior("student_t(3, 1, 10)", class = "Intercept"),  # default
           set_prior("gamma(0.01, 0.01)", class = "shape"),  # default
           set_prior("student_t(3, 0, 10)", class = "sd"),  # default
           set_prior("normal(1, 5)", class = "sd", group = "species"),
           set_prior("normal(3, 10)", class = "sd", coef = "Intercept", group = "species"), 
           set_prior("normal(5, 5)", class = "sd", coef = "propCon", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA", group = "species"),
           
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:Elevation", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:Elevation", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:fert", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:fert", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:text", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:text", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:steep", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:steep", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "propCon:NE", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA:NE", group = "species")
)

mod3 <- brm( sap_plot_count ~
               Elevation + fert + text + steep + NE +   # pop'n-level main effects
               plot_tree_BA + propCon +
               
               (1 + plot_tree_BA + propCon +                  # group-level main and interactive effects
                  plot_tree_BA:Elevation + propCon:Elevation + 
                  plot_tree_BA:fert + propCon:fert + 
                  plot_tree_BA:text + propCon:text + 
                  plot_tree_BA:steep + propCon:steep + 
                  plot_tree_BA:NE + propCon:NE | species),
               
             family = negbinomial(link = "log", link_shape = "log"),  
             # future = TRUE,
             save_all_pars = TRUE,  
             # control = list(adapt_delta = 0.9, stepsize = 0.01, max_treedepth = 15),
             prior = prior,
             init_r = 0.1,
             warmup = 1000,
             iter = 5000,
             chains = 4,
             data = dat
)
saveRDS(mod3, "chap3_mod3_Aug2021.RDS")

# model selection
k_mod3 <- kfold(mod3, K = 10)
saveRDS(k_mod3, "chap3_mod3_kfold_Aug2021.RDS")

se_elpd_diff <- function(diffs) {
  N <- length(diffs)
  # As `elpd_diff` is defined as the sum of N independent components,
  # we can compute the standard error by using the standard deviation
  # of the N components and multiplying by `sqrt(N)`.
  sqrt(N) * sd(diffs)
}

#### MODEL 2 ####
prior <- c(set_prior("normal(0, 1)", class = "b", coef= "Elevation"),
           set_prior("normal(0, 1)", class = "b", coef= "fert"),
           set_prior("normal(0, 1)", class = "b", coef= "text"),
           set_prior("normal(0, 1)", class = "b", coef= "steep"),
           set_prior("normal(0, 1)", class = "b", coef= "NE"),
           set_prior("normal(0, 1)", class = "b", coef= "plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "propCon"),
           
           set_prior("normal(0, 1)", class = "b", coef= "Elevation:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "Elevation:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "fert:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "fert:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "text:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "text:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "steep:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "steep:propCon"),
           set_prior("normal(0, 1)", class = "b", coef= "NE:plot_tree_BA"),
           set_prior("normal(0, 1)", class = "b", coef= "NE:propCon"),
           
           set_prior("student_t(3, 1, 10)", class = "Intercept"),  # default
           set_prior("gamma(0.01, 0.01)", class = "shape"),  # default
           set_prior("student_t(3, 0, 10)", class = "sd"),  # default
           set_prior("normal(1, 5)", class = "sd", group = "species"),
           set_prior("normal(3, 10)", class = "sd", coef = "Intercept", group = "species"), 
           set_prior("normal(5, 5)", class = "sd", coef = "propCon", group = "species"),
           set_prior("normal(1, 5)", class = "sd", coef = "plot_tree_BA", group = "species")
)

mod2 <- brm( sap_plot_count ~
               Elevation + fert + text + steep + NE +   # pop'n-level main effects
               plot_tree_BA + propCon +
               
               (1 + plot_tree_BA + propCon | species) +   # group-level main and interactive effects
               
               plot_tree_BA:Elevation + propCon:Elevation + 
               plot_tree_BA:fert + propCon:fert + 
               plot_tree_BA:text + propCon:text +
               plot_tree_BA:steep + propCon:steep + 
               plot_tree_BA:NE + propCon:NE,
             
             family = negbinomial(link = "log", link_shape = "log"),  
             # future = TRUE,
             save_all_pars = TRUE,  
             # control = list(adapt_delta = 0.9, stepsize = 0.01, max_treedepth = 15),
             prior = prior,
             init_r = 0.1,
             warmup = 1000,
             iter = 5000,
             chains = 4,
             data = dat
)
saveRDS(mod2, "chap3_mod2_Aug2021.RDS")

# model selection
k_mod2 <- kfold(mod2, K = 10)
saveRDS(k_mod2, "chap3_mod2_kfold_Aug2021.RDS")




#### MODEL 1 ####
prior <- c(set_prior("normal(0, 1)", class = "b", coef= "Elevation"),
           set_prior("normal(0, 1)", class = "b", coef= "fert"),
           set_prior("normal(0, 1)", class = "b", coef= "text"),
           set_prior("normal(0, 1)", class = "b", coef= "steep"),
           set_prior("normal(0, 1)", class = "b", coef= "NE"),
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

mod1 <- brm( sap_plot_count ~
               Elevation + fert + text + steep + NE +   # pop'n-level main effects
               plot_tree_BA + propCon +
               
               (1 + plot_tree_BA + propCon | species),   # group-level main and interactive effects
               
             family = negbinomial(link = "log", link_shape = "log"),  
             # future = TRUE,
             save_all_pars = TRUE,  
             # control = list(adapt_delta = 0.9, stepsize = 0.01, max_treedepth = 15),
             prior = prior,
             init_r = 0.1,
             warmup = 1000,
             iter = 5000,
             chains = 4,
             data = dat
)
saveRDS(mod1, "chap3_mod1_Aug2021.RDS")

# model selection
k_mod1 <- kfold(mod1, K = 10)
saveRDS(k_mod1, "chap3_mod1_kfold_Aug2021.RDS")
