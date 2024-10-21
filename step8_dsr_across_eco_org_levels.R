###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): visualizations of interest
###date(s): October 2024
###note(s): testing repo name change again again

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, lme4, corrplot, performance, ggeffects, ggpubr, parameters)

dat <- read_csv('local_data/dsr-eco-org-raw.csv') |> 
      rename(Program = program,
             Trophic_Group = troph_group,
             Species = scientific_name,
             Habitat = habitat,
             Site = site)

dat_scaled <- dat |> 
      select(program, troph_group, scientific_name, habitat, site, everything()) |> 
      group_by(program) |> 
      ## this is a function syntax
      mutate(across(comm_mean_bm:spp_bm_stability,\(x) scale(x, center = TRUE))) |>
      ungroup() |> 
      filter(spp_n_stability <=7,
             comm_n_stability >=-2.5) |> 
      rename(Program = program,
             Trophic_Group = troph_group,
             Species = scientific_name,
             Habitat = habitat,
             Site = site)
glimpse(dat_scaled)

# species richness on x axis ----------------------------------------------

model <- lm(spp_n_stability ~ mean_species_richness, data = dat_scaled)
r2 <- summary(model)$r.squared
spp_rich_spp_stab <- dat_scaled |> 
      ggplot(aes(x = mean_species_richness, y = spp_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Species Richness",
           y = "Scaled Species Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(troph_n_stability ~ mean_species_richness, data = dat_scaled)
r2 <- summary(model)$r.squared
spp_rich_troph_stab <- dat_scaled |> 
      ggplot(aes(x = mean_species_richness, y = troph_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Species Richness",
           y = "Scaled Trophic Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(comm_n_stability ~ mean_species_richness, data = dat_scaled)
r2 <- summary(model)$r.squared
spp_rich_comm_stab <- dat_scaled |> 
      ggplot(aes(x = mean_species_richness, y = comm_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Species Richness",
           y = "Scaled Community Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

p1 <- ggarrange(spp_rich_spp_stab, spp_rich_troph_stab, spp_rich_comm_stab,
                labels = c('a)','b)','c)'),
                # legend = 'bottom', common.legend = TRUE,
                ncol = 3, nrow = 1, align = "h", hjust = 0.5)

p1a <- annotate_figure(p1,
                       bottom = text_grob(label = "Scaled Species Richness",
                                        just = 'centre', rot = 0,
                                        color = "black", face = "bold"))

# species diversity on x axis ---------------------------------------------

model <- lm(spp_n_stability ~ mean_species_diversity, data = dat_scaled)
r2 <- summary(model)$r.squared
spp_div_spp_stab <- dat_scaled |> 
      ggplot(aes(x = mean_species_diversity, y = spp_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Species Diversity",
           y = "Scaled Species Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(troph_n_stability ~ mean_species_diversity, data = dat_scaled)
r2 <- summary(model)$r.squared
spp_div_troph_stab <- dat_scaled |> 
      ggplot(aes(x = mean_species_diversity, y = troph_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Species Diversity",
           y = "Scaled Trophic Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(comm_n_stability ~ mean_species_diversity, data = dat_scaled)
r2 <- summary(model)$r.squared
spp_div_comm_stab <- dat_scaled |> 
      ggplot(aes(x = mean_species_diversity, y = comm_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Species Diversity",
           y = "Scaled Community Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

p2 <- ggarrange(spp_div_spp_stab, spp_div_troph_stab, spp_div_comm_stab,
                labels = c('d)','e)','f)'),
                # legend = 'bottom', common.legend = TRUE,
                ncol = 3, nrow = 1, align = "h", hjust = 0.5)

p2a <- annotate_figure(p2,
                       bottom = text_grob(label = "Scaled Species Diversity",
                                          just = 'centre', rot = 0,
                                          color = "black", face = "bold"))

# trophic richness on x axis ----------------------------------------------

model <- lm(spp_n_stability ~ mean_trophic_richness, data = dat_scaled)
r2 <- summary(model)$r.squared
troph_rich_spp_stab <- dat_scaled |> 
      ggplot(aes(x = mean_trophic_richness, y = spp_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Trophic Richness",
           y = "Scaled Species Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(troph_n_stability ~ mean_trophic_richness, data = dat_scaled)
r2 <- summary(model)$r.squared
troph_rich_troph_stab <- dat_scaled |> 
      ggplot(aes(x = mean_trophic_richness, y = troph_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Trophic Richness",
           y = "Scaled Trophic Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(comm_n_stability ~ mean_trophic_richness, data = dat_scaled)
r2 <- summary(model)$r.squared
troph_rich_comm_stab <- dat_scaled |> 
      ggplot(aes(x = mean_trophic_richness, y = comm_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Trophic Richness",
           y = "Scaled Community Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

p3 <- ggarrange(troph_rich_spp_stab, troph_rich_troph_stab, troph_rich_comm_stab,
                labels = c('g)','h)','i)'),
                # legend = 'bottom', common.legend = TRUE,
                ncol = 3, nrow = 1, align = "h", hjust = 0.5)

p3a <- annotate_figure(p3,
                       bottom = text_grob(label = "Scaled Trophic Richness",
                                          just = 'centre', rot = 0,
                                          color = "black", face = "bold"))

# trophic diversity on x axis ----------------------------------------------

model <- lm(spp_n_stability ~ mean_trophic_diversity, data = dat_scaled)
r2 <- summary(model)$r.squared
troph_div_spp_stab <- dat_scaled |> 
      ggplot(aes(x = mean_trophic_diversity, y = spp_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Trophic Diversity",
           y = "Scaled Species Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(troph_n_stability ~ mean_trophic_diversity, data = dat_scaled)
r2 <- summary(model)$r.squared
troph_div_troph_stab <- dat_scaled |> 
      ggplot(aes(x = mean_trophic_diversity, y = troph_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Trophic Diversity",
           y = "Scaled Trophic Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(comm_n_stability ~ mean_trophic_diversity, data = dat_scaled)
r2 <- summary(model)$r.squared
troph_div_comm_stab <- dat_scaled |> 
      ggplot(aes(x = mean_trophic_diversity, y = comm_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Trophic Diversity",
           y = "Scaled Community Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

p4 <- ggarrange(troph_div_spp_stab, troph_div_troph_stab, troph_div_comm_stab,
                labels = c('j)','k)','l)'),
                # legend = 'bottom', common.legend = TRUE,
                ncol = 3, nrow = 1, align = "h", hjust = 0.5)

p4a <- annotate_figure(p4,
                       bottom = text_grob(label = "Scaled Trophic Diversity",
                                          just = 'centre', rot = 0,
                                          color = "black", face = "bold"))

p5 <- ggarrange(p1a,p2a,p3a,p4a,
                legend = 'bottom', common.legend = TRUE,
                ncol = 1, nrow = 4, align = 'h')

p5a <- annotate_figure(p5,
                       left = text_grob(label = 'Scaled Nitrogen Supply Stability (1/CV)',
                                        just = 'centre', rot = 90, vjust = -.5, 
                                        color = "black", face = "bold"))

# ggsave("output/ms-second-round/plots/twelve-panel-dsr-across-ecological-levels.tiff", units = "in", width = 12,
#        height = 12, dpi =  600, compression = "lzw")


# turnover on x axis ------------------------------------------------------

model <- lm(spp_n_stability ~ beta_time, data = dat_scaled)
r2 <- summary(model)$r.squared
turnover_spp_stab <- dat_scaled |> 
      ggplot(aes(x = beta_time, y = spp_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Temporal Turnover",
           y = "Scaled Species Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(troph_n_stability ~ beta_time, data = dat_scaled)
r2 <- summary(model)$r.squared
turnover_troph_stab <- dat_scaled |> 
      ggplot(aes(x = beta_time, y = troph_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Temporal Turnover",
           y = "Scaled Trophic Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(comm_n_stability ~ beta_time, data = dat_scaled)
r2 <- summary(model)$r.squared
turnover_comm_stab <- dat_scaled |> 
      ggplot(aes(x = beta_time, y = comm_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Temporal Turnover",
           y = "Scaled Community Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

p6 <- ggarrange(turnover_spp_stab, turnover_troph_stab, turnover_comm_stab,
                labels = c('a)','b)','c)'),
                # legend = 'bottom', common.legend = TRUE,
                ncol = 3, nrow = 1, align = "h", hjust = 0.5)

p6a <- annotate_figure(p6,
                       bottom = text_grob(label = "Scaled Temporal Turnover",
                                          just = 'centre', rot = 0,
                                          color = "black", face = "bold"))

# synchrony on x axis -----------------------------------------------------

model <- lm(spp_n_stability ~ synch, data = dat_scaled)
r2 <- summary(model)$r.squared
synch_spp_stab <- dat_scaled |> 
      ggplot(aes(x = synch, y = spp_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Temporal Synchrony",
           y = "Scaled Species Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(troph_n_stability ~ synch, data = dat_scaled)
r2 <- summary(model)$r.squared
synch_troph_stab <- dat_scaled |> 
      ggplot(aes(x = synch, y = troph_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Temporal Synchrony",
           y = "Scaled Trophic Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

model <- lm(comm_n_stability ~ synch, data = dat_scaled)
r2 <- summary(model)$r.squared
synch_comm_stab <- dat_scaled |> 
      ggplot(aes(x = synch, y = comm_n_stability)) +
      geom_point(fill = 'white', color = 'darkgrey', stroke = 0.5) + 
      geom_smooth(method = "lm", color = 'black', linewidth = 2, se = FALSE) + 
      geom_smooth(aes(color = Program), method = 'lm', se = FALSE) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2, 3)),
               vjust = 1.5, hjust = 1.1, size = 5, color = "black", fontface = "bold") +
      labs(x = "Scaled Temporal Synchrony",
           y = "Scaled Community Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            # axis.title.y = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

p7 <- ggarrange(synch_spp_stab, synch_troph_stab, synch_comm_stab,
                labels = c('d)','e)','f)'),
                # legend = 'bottom', common.legend = TRUE,
                ncol = 3, nrow = 1, align = "h", hjust = 0.5)

p7a <- annotate_figure(p7,
                       bottom = text_grob(label = "Scaled Temporal Synchrony",
                                          just = 'centre', rot = 0,
                                          color = "black", face = "bold"))

p8 <- ggarrange(p6a,p7a,
                legend = 'bottom', common.legend = TRUE,
                ncol = 1, nrow = 2, align = 'h')

p8a <- annotate_figure(p8,
                       left = text_grob(label = 'Scaled Nitrogen Supply Stability (1/CV)',
                                        just = 'centre', rot = 90, vjust = -.5, 
                                        color = "black", face = "bold"))

# ggsave("output/ms-second-round/plots/six-panel-mechanisms-across-ecological-levels.tiff", units = "in", width = 12,
#        height = 12, dpi =  600, compression = "lzw")
