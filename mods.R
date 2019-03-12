
require(dplyr)
require(rstanarm)
require(lme4)
require(MASS)
require(gam)

dat <- read.csv("chap3_data_by_plot.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
dat <- dat %>% filter(species_name != "other")

# try using negative binomial distribution with MASS package
mod <- glm.nb(sap_plot_count ~ tmax + mean_prec + comp1 + comp2 + #species +
            plot_tree_BA + plot_cons_tree_BA + 
            # plot_tree_BA:species + plot_cons_tree_BA:species +
            plot_tree_BA:tmax + plot_cons_tree_BA:tmax +
            plot_tree_BA:mean_prec + plot_cons_tree_BA:mean_prec +
            plot_tree_BA:comp1 + plot_cons_tree_BA:comp1 +
            plot_tree_BA:comp2 + plot_cons_tree_BA:comp2,
            # plot_tree_BA:tmax:species + plot_cons_tree_BA:tmax:species +
            # plot_tree_BA:mean_prec:species + plot_cons_tree_BA:mean_prec:species +
            # plot_tree_BA:comp1:species + plot_cons_tree_BA:comp1:species +
            # plot_tree_BA:comp2:species + plot_cons_tree_BA:comp2:species,
            control=glm.control(maxit=1000),
          data = dat)
summary(mod)
coef <- as.data.frame(summary(mod)$coefficients)



# try rstanarm
# dat <- dat %>% filter(!is.na(comp1), !is.na(comp2), !is.na(tmin), !is.na(mean_prec))
# scaled_vars <- c("tmin", "tmax", "mean_prec", "mean_vp", "mean_srad", 
#                  "tot_mod_sap_count", "plot_tree_BA", "plot_cons_tree_BA")
# dat[, scaled_vars] <- scale(dat[, scaled_vars])  # scale numeric variables

mod <- stan_glmer.nb(sap_plot_count ~
                    plot_tree_BA + plot_cons_tree_BA + 
                    tmin + 
                    species +
#                    plot_tree_BA:species + plot_cons_tree_BA:species +
           plot_tree_BA:tmin:species + plot_cons_tree_BA:tmin:species +
#             plot_tree_BA*mean_prec + plot_cons_tree_BA*mean_prec + 
#             plot_tree_BA*comp1 + plot_cons_tree_BA*comp1 + 
#             plot_tree_BA*comp2 + plot_cons_tree_BA*comp2, 
               chains = 1,
               family = neg_binomial_2(link),  # i think this is the default
                data = dat)

coefs <- names(mod$coefficients)
coefs2 <- coefs[grep("Plot", coefs, invert = TRUE)]
coef_table <- as.data.frame(posterior_interval(mod, prob=0.95, pars = coefs2))
write.csv(coef_table, "chap3_CIs.csv")



# try generalized additive modeling
# haven't run this successfully yet
gam1 <- gam(sap_plot_count ~ s(tmax) + s(mean_prec) + s(comp1) + s(comp2) +
              s(plot_tree_BA) + s(plot_cons_tree_BA) + 
              s(plot_tree_BA:tmax) + s(plot_cons_tree_BA:tmax) +
              s(plot_tree_BA:mean_prec) + s(plot_cons_tree_BA:mean_prec) +
              s(plot_tree_BA:comp1) + s(plot_cons_tree_BA:comp1) +
              s(plot_tree_BA:comp2) + s(plot_cons_tree_BA:comp2),
            family = poisson(link = "log"),
            data = dat)
summary(gam1)


