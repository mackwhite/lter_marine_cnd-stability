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

troph_turnover_effect_dsr <- ggpredict(test, 
                                       type = "re",
                                       terms = c('beta_time[-2.1:3.4 by=0.1]', 'Program')
                                       )
plot(troph_turnover_effect_dsr)

random_slopes <- ranef(test)$Program[, , "troph_beta_time"]
random_slopes_df <- as.data.frame(random_slopes)
random_slopes_df$group <- rownames(random_slopes_df)  # Add group names
colnames(random_slopes_df) <- c("random_slope", "group")

ggplot(random_slopes_df, aes(x = group, y = random_slope)) +
      geom_point() +
      labs(title = "Random Slopes for Each Group",
           x = "Group",
           y = "Random Slope for x") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
