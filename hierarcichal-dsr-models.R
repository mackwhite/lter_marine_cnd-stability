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
      select(Program, Trophic_Group, Species, Habitat, Site, comm_n_stability, everything()) |> 
      mutate(comm_n_stability = scale(comm_n_stability)) |> 
      group_by(Program) |> 
      ## this is a function syntax
      mutate(across(comm_mean_bm:troph_synch,\(x) scale(x, center = TRUE))) |>
      ungroup()

glimpse(dat_scaled)

dat_ready <- dat_scaled |> 
      select(Program, Habitat, Site, 
             comm_mean_max_ss,
             comm_mean_bm, comm_sd_bm, comm_bm_stability,
             comm_mean_n, comm_sd_n, comm_n_stability,
             comm_mean_p, comm_sd_p, comm_p_stability,
             mean_species_richness, mean_species_diversity, 
             mean_trophic_richness, mean_trophic_diversity,
             beta_time, synch,
             troph_beta_time, troph_synch) |> 
      distinct()

# hierarchical models with forward selection ------------------------------

### round one ---

m1 <- glmmTMB(comm_n_stability ~ comm_mean_max_ss + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m2 <- glmmTMB(comm_n_stability ~ mean_species_richness + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m3 <- glmmTMB(comm_n_stability ~ mean_species_diversity + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m4 <- glmmTMB(comm_n_stability ~ mean_trophic_richness + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m5 <- glmmTMB(comm_n_stability ~ mean_trophic_diversity + (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m6 <- glmmTMB(comm_n_stability ~ (1|Program), data = dat_ready,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

rd1_model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6)

rd1_aicc_richness <- rd1_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

### round two ---

m11 <- glmmTMB(comm_n_stability ~ mean_trophic_diversity + comm_mean_max_ss + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

rd2_model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6,m11)

rd2_aicc_richness <- rd2_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))
### model 5 beats model m11 as a result of parsimony

performance(m5)
performance::compare_performance(m1,m2,m3,m4,m5,m6,m11)
summary(m5)
summary(dat_ready)
ggcoef_table(m5)
## save table as svg
# ggsave("output/ms-second-round/plots/ggcoef-DSR-bestfit.svg", units = "in", width = 10,
#        height = 6, dpi =  600)

troph_effect_dsr <- ggemmeans(m5, 'mean_trophic_diversity[-3.2:2 by=0.25]')
plot(troph_effect_dsr)

# second step - investigating effects of synchrony and turnover -----------

sm1 <- glmmTMB(comm_n_stability ~ mean_trophic_diversity + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

sm2 <- glmmTMB(comm_n_stability ~ beta_time + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

sm3 <- glmmTMB(comm_n_stability ~ synch + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

sm4 <- glmmTMB(comm_n_stability ~ troph_beta_time + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

sm5 <- glmmTMB(comm_n_stability ~ troph_synch + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

s2rd1_model_table <- performance::compare_performance(sm1,sm2,sm3,sm4,sm5)

s2rd1_aicc_richness <- s2rd1_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

### step 2, round one 2

sm31 <- glmmTMB(comm_n_stability ~ synch + beta_time + (1|Program), data = dat_ready,
               # family = gaussian(link = "log"),
               control = glmmTMBControl(optimizer=optim,
                                        optArgs = list(method = 'CG')),
               REML = FALSE)

sm32 <- glmmTMB(comm_n_stability ~ synch + mean_trophic_diversity + (1|Program), data = dat_ready,
                # family = gaussian(link = "log"),
                control = glmmTMBControl(optimizer=optim,
                                         optArgs = list(method = 'CG')),
                REML = FALSE)

sm33 <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + (1|Program), data = dat_ready,
                # family = gaussian(link = "log"),
                control = glmmTMBControl(optimizer=optim,
                                         optArgs = list(method = 'CG')),
                REML = FALSE)

sm34 <- glmmTMB(comm_n_stability ~ synch + troph_synch + (1|Program), data = dat_ready,
                # family = gaussian(link = "log"),
                control = glmmTMBControl(optimizer=optim,
                                         optArgs = list(method = 'CG')),
                REML = FALSE)

s2rd2_model_table <- performance::compare_performance(sm1,sm2,sm3,sm4,sm5,sm32,sm33,sm34)

s2rd2_aicc_richness <- s2rd2_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

### step 2, round 3

sm331 <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + mean_trophic_diversity + (1|Program), data = dat_ready,
                # family = gaussian(link = "log"),
                control = glmmTMBControl(optimizer=optim,
                                         optArgs = list(method = 'CG')),
                REML = FALSE)

sm332 <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + troph_synch + (1|Program), data = dat_ready,
                 # family = gaussian(link = "log"),
                 control = glmmTMBControl(optimizer=optim,
                                          optArgs = list(method = 'CG')),
                 REML = FALSE)

sm333 <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + beta_time + (synch+troph_beta_time+beta_time|Program), data = dat_ready,
                 # family = gaussian(link = "log"),
                 control = glmmTMBControl(optimizer=optim,
                                          optArgs = list(method = 'CG')),
                 REML = FALSE)
check <- mixedup::extract_random_coefs(sm333)
performance::check_model(sm333)
diagnose(sm333, check_hessian = FALSE)

check_nozero <- check |> 
      mutate(nozero = map2_lgl(lower_2.5, upper_97.5,\(x,y) between(0,x,y)))
overall <- mixedup::extract_fixed_effects(sm333)
performance::check_collinearity(sm333)

s2rd3_model_table <- performance::compare_performance(sm3,sm33,sm331,sm332,sm333)

s2rd3_aicc_richness <- s2rd3_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

### round four ---
sm3331 <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + beta_time + mean_trophic_diversity + (1|Program), data = dat_ready,
                 # family = gaussian(link = "log"),
                 control = glmmTMBControl(optimizer=optim,
                                          optArgs = list(method = 'CG')),
                 REML = FALSE)

sm3332 <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + beta_time + troph_synch + (1|Program), data = dat_ready,
                  # family = gaussian(link = "log"),
                  control = glmmTMBControl(optimizer=optim,
                                           optArgs = list(method = 'CG')),
                  REML = FALSE)

s2rd4_model_table <- performance::compare_performance(sm1,sm2,sm3,sm4,sm5,
                                                      sm31,sm32,sm33,sm34,
                                                      sm331,sm332,sm333,
                                                      sm3331,sm3332)
s2rd4_aicc_richness <- s2rd4_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

performance(sm333)
performance::compare_performance(sm1,sm2,sm3,sm4,sm5,
                                 sm31,sm32,sm33,sm34,
                                 sm331,sm332,sm333,
                                 sm3331,sm3332)
summary(sm333)
summary(dat_ready)
performance::check_collinearity(sm333)
ggcoef_table(sm333)

### save table as svg
ggsave("output/ms-second-round/plots/ggcoef-dsr-mechanism-bestfit.svg", units = "in", width = 10,
       height = 6, dpi =  600)

synch_effect_dsr <- ggemmeans(sm333, 'synch[-2:3.2 by=0.25]')
plot(synch_effect_dsr)

turnover_effect_dsr <- ggemmeans(sm333, 'beta_time[-3:3 by=0.25]')
plot(turnover_effect_dsr)

troph_turnover_effect_dsr <- ggemmeans(sm333, 'troph_beta_time[-2.1:3.4 by=0.25]')
plot(troph_turnover_effect_dsr)

sm333check <- glmmTMB(comm_n_stability ~ synch + troph_beta_time + beta_time, data = dat_ready,
                 # family = gaussian(link = "log"),
                 control = glmmTMBControl(optimizer=optim,
                                          optArgs = list(method = 'CG')),
                 REML = FALSE)
performance::check_collinearity(sm333check)
performance::compare_performance(sm333,sm333check)
