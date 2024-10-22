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
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, lme4, corrplot, performance, ggeffects, ggpubr, parameters, ggstats)

dat <- read_csv('local_data/dsr-eco-org-raw.csv') |> 
      rename(Program = program,
             Trophic_Group = troph_group,
             Species = scientific_name,
             Habitat = habitat,
             Site = site)

dat_scaled <- dat |> 
      select(Program, Trophic_Group, Species, Habitat, Site, everything()) |> 
      group_by(Program) |> 
      ## this is a function syntax
      mutate(across(comm_mean_bm:spp_bm_stability,\(x) scale(x, center = TRUE))) |>
      ungroup()
glimpse(dat_scaled)

dat_ready <- dat_scaled |> 
      select(Program, Habitat, Site, 
             comm_mean_max_ss,
             comm_mean_bm, comm_sd_bm, comm_bm_stability,
             comm_mean_n, comm_sd_n, comm_n_stability,
             comm_mean_p, comm_sd_p, comm_p_stability,
             mean_species_richness, mean_species_diversity, 
             mean_trophic_richness, mean_trophic_diversity) |> 
      distinct()

# hierarchical models with forward selection ------------------------------

### round one ---

m1 <- glmmTMB(comm_mean_n ~ comm_mean_max_ss + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m2 <- glmmTMB(comm_mean_n ~ mean_species_richness + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m3 <- glmmTMB(comm_mean_n ~ mean_species_diversity + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m4 <- glmmTMB(comm_mean_n ~ mean_trophic_richness + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m5 <- glmmTMB(comm_mean_n ~ mean_trophic_diversity + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m6 <- glmmTMB(comm_mean_n ~ (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

rd1_model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6)

rd1_aicc_richness <- rd1_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

### round two ---

m11 <- glmmTMB(comm_mean_n ~ mean_species_richness + comm_mean_max_ss + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

rd2_model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6,m11)

rd2_aicc_richness <- rd2_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

performance(m11)
performance::compare_performance(m1,m2,m3,m4,m5,m6,m11)
summary(m11)
performance::check_collinearity(m11)
summary(dat_ready)
ggcoef_model(m11)
ggcoef_table(m11)
### save table as svg
# ggsave("output/ms-second-round/plots/ggcoef-BEF-bestfit.svg", units = "in", width = 10,
#        height = 6, dpi =  600)

spp_effect_bef <- ggemmeans(m11, 'mean_species_richness[-3:2 by=0.25]')
plot(spp_effect_bef)

size_effect_bef <- ggemmeans(m11, 'comm_mean_max_ss[-2:7 by=0.25]')
plot(size_effect_bef)


