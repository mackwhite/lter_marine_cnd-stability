###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): visualize scaled dsr relationships across various scales of organization
###date(s): October 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, MuMIn, sjPlot, lme4, corrplot, 
                 performance, ggeffects, ggpubr, parameters, ggstats, brms, mixedup)
remotes::install_github('m-clark/mixedup')

pr = prior(normal(0,1), class = 'b')
test <- brm(comm_n_stability ~ synch + troph_beta_time + beta_time + (synch+troph_beta_time+beta_time|Program),
            data = dat_ready,
            prior = pr, warmup = 1000, iter = 10000, chains = 4)
summary(test)

ranef(test)
pp_check(test)
check <- mixedup::extract_random_coefs(test)
overall <- mixedup::extract_fixed_effects(test)
ggcoef_table(test)

check_nozero <- check |> 
      mutate(nozero = map2_lgl(lower_2.5, upper_97.5,\(x,y) between(0,x,y)))

test2 <- brm(comm_n_stability ~ synch + troph_beta_time + beta_time + (mean_trophic_diversity|Program),
            data = dat_ready,
            prior = pr, warmup = 1000, iter = 10000, chains = 4)

performance::compare_performance(test, test2)
raranef()

waic_fit1 <- waic(test)
waic_fit2 <- waic(test2)
compare <- loo_compare(waic_fit1, waic_fit2)

random_effects <- ranef(test2)
ggcoef_table(test)


# View the random slopes for 'x' across groups
random_effects$Program[, , "mean_trophic_diversity"]
