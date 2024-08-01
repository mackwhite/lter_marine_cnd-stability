###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): hierarchical models for species evenness and richness
###date(s): June 2024
###note(s): testing repo name change again again

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, lme4, corrplot, performance, ggeffects, ggpubr, parameters)

dat <- read_csv("local_data/final_model_data_08012024.csv")

model_data_scaled <- dat |> 
  group_by(program) |> 
  ## this is a function syntax
  mutate(across(mean_max_ss:synch,\(x) scale(x, center = TRUE))) |>
  ungroup() |>
  mutate(n_stability_scaled = scale(n_stability))

model_data_scaledv2 <- dat |> 
      group_by(program, habitat) |> 
      ## this is a function syntax
      mutate(across(mean_max_ss:synch,\(x) scale(x, center = TRUE))) |>
      ungroup() |>
      mutate(n_stability_scaled = scale(n_stability))

###########################################################################
# diversity-stability plots at program ------------------------------------
###########################################################################

# Species Richness Plots --------------------------------------------------

rich_scaled <- model_data_scaled |> 
  ggplot(aes(x = mean_species_richness, y = n_stability_scaled, color = program)) +
  geom_point() +  # Adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
  labs(x = "Scaled Species Richness",
       y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

rich_scaledv2 <- model_data_scaledv2 |> 
      ggplot(aes(x = mean_species_richness, y = n_stability_scaled, color = program)) +
      geom_point() +  # Adds the scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Scaled Species Richness",
           y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_text(face = "bold", color = "black"),
            legend.position = "bottom",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

# Species Evenness Plots --------------------------------------------------

div_scaled <- model_data_scaled |> 
  rename(Program = program) |> 
  ggplot(aes(x = mean_species_diversity, y = n_stability_scaled, color = Program)) +
  geom_point() +  # Adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
  labs(x = "Scaled Species Diversity (Inverse Simpson)",
       y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

div_scaledv2 <- model_data_scaledv2 |> 
      rename(Program = program) |> 
      ggplot(aes(x = mean_species_diversity, y = n_stability_scaled, color = Program)) +
      geom_point() +  # Adds the scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Scaled Species Diversity (Inverse Simpson)",
           y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.background = element_rect(color = "black"),
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

# Trophic Diversity Plots --------------------------------------------------

trophic_div_scaled <- model_data_scaled |> 
      rename(Program = program) |> 
      ggplot(aes(x = mean_trophic_diversity, y = n_stability_scaled, color = Program)) +
      geom_point() +  # Adds the scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Scaled Trophic Diversity (Inverse Simpson)",
           y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.background = element_rect(color = "black"),
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

ggarrange(rich_scaled, div_scaled, trophic_div_scaled,
          labels = c('a)','b)', 'c)'),
          ncol = 3, nrow = 1, align = "h")

# ggsave("output/ms first round/plots/two-panel-dsr-meeting_UPDATED07192024.tiff", units = "in", width = 10,
#        height = 6, dpi =  600, compression = "lzw")

# ggsave("output/ms first round/plots/two-panel-dsr-meeting-legend.tiff", units = "in", width = 10,
#        height = 6, dpi =  600, compression = "lzw")

###########################################################################
# beta/synch-stability plots at program ------------------------------------
###########################################################################
synch_scaled <- model_data_scaled |> 
      rename(Program = program) |> 
      ggplot(aes(x = synch, y = n_stability_scaled, color = Program)) +
      geom_point() +  # Adds the scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Scaled Species Synchrony",
           y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.background = element_rect(color = "black"),
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

turnover_scaled <- model_data_scaled |> 
      rename(Program = program) |> 
      ggplot(aes(x = beta_time, y = n_stability_scaled, color = Program)) +
      geom_point() +  # Adds the scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Scaled Species Turnover",
           y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.background = element_rect(color = "black"),
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

###########################################################################
# hierarchical models -----------------------------------------------------
###########################################################################

m1 <- glmmTMB(n_stability_scaled ~ mean_species_richne + (mean_spp_rich|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
model_performance(m1)
summary(m1)

m2 <- glmmTMB(n_stability_scaled ~ mean_spp_rich + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m2)
# summary(m2)

m3 <- glmmTMB(n_stability_scaled ~ (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m3)
# summary(m3)

m4 <- glmmTMB(n_stability_scaled ~ mean_species_diversity + (mean_species_diversity|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m4)
# summary(m4)

m5 <- glmmTMB(n_stability_scaled ~ mean_species_diversity + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m5)
# summary(m5)

m6 <- glmmTMB(n_stability_scaled ~ (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m6)
# summary(m6)

model_table_richness <- performance::compare_performance(m1,m2,m3)
model_table_evenness <- performance::compare_performance(m4,m5,m6)

aicc_richness <- model_table_richness |> 
  mutate(delta_aicc = AICc - min(AICc))

aicc_evenness <- model_table_evenness |> 
  mutate(delta_aicc = AICc - min(AICc))

# full global models ------------------------------------------------------

global_model_N <- glmmTMB(
  n_stability_scaled ~ 
    # ecosystem +
    # latitude +
    mean_max_ss +
    mean_species_richness + 
    mean_species_diversity +
    mean_trophic_diversity + 
    beta_time + synch +
   (1|program),
  data = model_data_scaled,
  na.action = "na.fail",
  # family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_N)
performance::check_model(global_model_N)

model_set_N <- dredge(global_model_N,
                      # subset = !(`cond(beta_time)`&&`cond(synch)`)
                      subset = !(`cond(mean_species_richness)`&&`cond(mean_species_diversity)`)
                      ) |>
  filter(delta < 4)

# hierarchical model fitting ----------------------------------------------

m1 <- glmmTMB(n_stability_scaled ~ mean_max_ss + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m2 <- glmmTMB(n_stability_scaled ~ mean_species_richness + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m3 <- glmmTMB(n_stability_scaled ~ mean_species_diversity + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m4 <- glmmTMB(n_stability_scaled ~ mean_trophic_diversity + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m5 <- glmmTMB(n_stability_scaled ~ beta_time + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m6 <- glmmTMB(n_stability_scaled ~ synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

rd1_model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6)

rd1_aicc_richness <- rd1_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

# round two ---------------------------------------------------------------

m12 <- glmmTMB(n_stability_scaled ~ mean_max_ss + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m22 <- glmmTMB(n_stability_scaled ~ mean_species_richness + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m32 <- glmmTMB(n_stability_scaled ~ mean_species_diversity + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m42 <- glmmTMB(n_stability_scaled ~ mean_trophic_diversity + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m52 <- glmmTMB(n_stability_scaled ~ beta_time + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m62 <- glmmTMB(n_stability_scaled ~ synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

rd2_model_table <- performance::compare_performance(m12,m22,m32,m42,m52,m62)

rd2_aicc_richness <- rd2_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))

# round three -------------------------------------------------------------

m13 <- glmmTMB(n_stability_scaled ~ mean_max_ss + beta_time + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m23 <- glmmTMB(n_stability_scaled ~ mean_species_richness + beta_time + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m33 <- glmmTMB(n_stability_scaled ~ mean_species_diversity + beta_time + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m43 <- glmmTMB(n_stability_scaled ~ mean_trophic_diversity + beta_time + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

m53 <- glmmTMB(n_stability_scaled ~ beta_time + synch + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)

rd3_model_table <- performance::compare_performance(m13,m23,m33,m43,m53)

rd3_aicc_richness <- rd3_model_table |> 
      mutate(delta_aicc = AICc - min(AICc))
