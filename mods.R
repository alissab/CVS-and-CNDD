
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
nrow(species_num[species_num$n_plots>=100,])  # 80 species are on at least 100 plots
dat <- left_join(dat, species_num, by="species")
dat <- dat %>% filter(n_plots >= 500)  # leaves you with 31,533 rows

# scale numeric data  DO YOU NEED TO DO THIS BEFORE RUNNING BRM? 
# scaled_vars <- c("tmax", "mean_prec", "moisture",
#                  "tot_mod_sap_count", "plot_tree_BA", "plot_cons_tree_BA")
# dat[, scaled_vars] <- scale(dat[, scaled_vars])


plan(cluster)  # for parallel computing on Longleaf cluster
mod <- brm(sap_plot_count ~
            tmax + mean_prec + moisture + (1|species) +
            plot_tree_BA + plot_cons_tree_BA,
            # plot_tree_BA:species + plot_cons_tree_BA:species +
            plot_tree_BA:tmax + plot_cons_tree_BA:tmax +
            plot_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec +
            plot_tree_BA:moisture + plot_cons_tree_BA:moisture,
            # plot_tree_BA:tmax:species + plot_cons_tree_BA:tmax:species +
            # plot_tree_BA:mean_prec:species + plot_cons_tree_BA:mean_prec:species + 
            # plot_tree_BA:moisture:species + plot_cons_tree_BA:moisture:species,
  family = negbinomial(link = "log", link_shape = "log"),  # default link function
  future = TRUE,  # parallel computing, one core per chain
  save_all_pars = TRUE,  # I think you need this for plotting posteriors?
  control = list(adapt_delta = 0.999, max_treedepth = 15), 
  data = dat
)

# stancode(mod)  # gives you model code in STAN
# standata(mod)  # gives you model data
# summary(mod, waic = TRUE)  # gives you model summary
# ranef(mod)  # gives you group-level model estimates

save.image("chap3_hardw_mods.RData")

# you can also specify smoothing terms a la GAMs: s(term) OR t2(term1, term2)
# you can specify measurement error: me(term, #)
# you can impute missing data on the fly: mi(term) + term|mi() ~ z


# using mgcv
# NOTE: you can specify smoothed functions of interactions: ti(plot_tree_BA, species)
# mod <- gam(sap_plot_count ~ tmax + mean_prec + species +
#              s(plot_tree_BA) + s(plot_cons_tree_BA) + 
#              plot_tree_BA:species + plot_cons_tree_BA:species +
#              ti(plot_tree_BA, tmax) + ti(plot_cons_tree_BA, tmax) +
#              ti(plot_tree_BA, mean_prec) + ti(plot_cons_tree_BA, mean_prec) +
#              plot_tree_BA:tmax:species + plot_cons_tree_BA:tmax:species +
#              plot_tree_BA:mean_prec:species + plot_cons_tree_BA:mean_prec:species,
#            family = negbin(3),
#            data = dat)
# 
# p_table <- data.frame(mod$p.table)
# p_table <- within(p_table, {lci <- Estimate - qnorm(0.975) * Std..Error
# uci <- Estimate + qnorm(0.975) * Std..Error})



# using negative binomial distribution with MASS package
# mod <- glm.nb(sap_plot_count ~ tmax + mean_prec + species + #comp1 + comp2 + 
#             plot_tree_BA + plot_cons_tree_BA + 
#             plot_tree_BA:species + plot_cons_tree_BA:species +
#             plot_tree_BA:tmax + plot_cons_tree_BA:tmax +
#             plot_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec,
#             # plot_tree_BA:comp1 + plot_cons_tree_BA:comp1 +
#             # plot_tree_BA:comp2 + plot_cons_tree_BA:comp2 +
#             # plot_tree_BA:tmax:species + plot_cons_tree_BA:tmax:species +
#             # plot_tree_BA:mean_prec:species + plot_cons_tree_BA:mean_prec:species +
#             # plot_tree_BA:comp1:species + plot_cons_tree_BA:comp1:species +
#             # plot_tree_BA:comp2:species + plot_cons_tree_BA:comp2:species,
#             control=glm.control(maxit=1000),
#           data = dat)
# summary(mod)
# coef <- as.data.frame(summary(mod)$coefficients)

