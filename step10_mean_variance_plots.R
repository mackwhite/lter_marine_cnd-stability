###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): visualize scaled relationships between mean and sd to look for overyielding vs complimentatiry effects
###date(s): October 2024
###note(s): 

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
      select(Program, Trophic_Group, Species, Habitat, Site, everything()) |> 
      group_by(Program) |> 
      ## this is a function syntax
      mutate(across(comm_mean_bm:spp_bm_stability,\(x) scale(x, center = TRUE))) |>
      ungroup() |> 
      filter(spp_n_stability <=7,
             comm_n_stability >=-2.5,
             comm_mean_n )
glimpse(dat_scaled)

# dat_scaled |> 
#       ggplot(x = mean_species_richness) +
#       geom_point(aes(y = comm_mean_n, x = mean_species_richness), color = "black") +
#       geom_point(aes(y = comm_sd_n, x = mean_species_richness), color = "darkgrey") +
#       geom_smooth(aes(y = comm_mean_n, x = mean_species_richness), 
#                   method = "lm", color = "black", se = FALSE) +
#       geom_smooth(aes(y = comm_sd_n, x = mean_species_richness), 
#                   method = "lm", color = "darkgrey", se = FALSE) +
#       facet_wrap(~Program)
# 
# dat_scaled |> 
#       ggplot(x = mean_species_diversity) +
#       geom_point(aes(y = comm_mean_n, x = mean_species_diversity), color = "black") +
#       geom_point(aes(y = comm_sd_n, x = mean_species_diversity), color = "darkgrey") +
#       geom_smooth(aes(y = comm_mean_n, x = mean_species_diversity), 
#                   method = "lm", color = "black", se = FALSE) +
#       geom_smooth(aes(y = comm_sd_n, x = mean_species_diversity), 
#                   method = "lm", color = "darkgrey", se = FALSE) +
#       facet_wrap(~Program)
# 
# dat_scaled |> 
#       ggplot(x = mean_trophic_richness) +
#       geom_point(aes(y = comm_mean_n, x = mean_trophic_richness), color = "black") +
#       geom_point(aes(y = comm_sd_n, x = mean_trophic_richness), color = "darkgrey") +
#       geom_smooth(aes(y = comm_mean_n, x = mean_trophic_richness), 
#                   method = "lm", color = "black", se = FALSE) +
#       geom_smooth(aes(y = comm_sd_n, x = mean_trophic_richness), 
#                   method = "lm", color = "darkgrey", se = FALSE) +
#       facet_wrap(~Program)
# 
# dat_scaled |> 
#       ggplot(x = mean_trophic_diversity) +
#       geom_point(aes(y = comm_mean_n, x = mean_trophic_diversity), color = "black") +
#       geom_point(aes(y = comm_sd_n, x = mean_trophic_diversity), color = "darkgrey") +
#       geom_smooth(aes(y = comm_mean_n, x = mean_trophic_diversity), 
#                   method = "lm", color = "black", se = FALSE) +
#       geom_smooth(aes(y = comm_sd_n, x = mean_trophic_diversity), 
#                   method = "lm", color = "darkgrey", se = FALSE) +
#       facet_wrap(~Program)

combined <- dat_scaled |> 
      ggplot(x = mean_trophic_diversity) +
      geom_point(aes(y = comm_mean_n, x = mean_trophic_diversity, color = "mean")) +
      geom_point(aes(y = comm_sd_n, x = mean_trophic_diversity, color = "sd")) +
      geom_smooth(aes(y = comm_mean_n, x = mean_trophic_diversity, color = "mean"), 
                  method = "lm", se = FALSE) +
      geom_smooth(aes(y = comm_sd_n, x = mean_trophic_diversity, color = "sd"), 
                  method = "lm", se = FALSE) +
      # facet_wrap(~Program, scales = "free") +
      labs(x = "Scaled Mean Trophic Diversity",
           y = "Scaled Temporal Mean and SD of Community Nitrogen Supply",
           color = 'measurement') +
      scale_color_manual(values = c("mean" = "black", "sd" = "darkgrey")) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            # axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold", color = "black"),
            # axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))
ggsave("output/ms-second-round/plots/one-panel-mean-variance-BEF-sites-combined.tiff", units = "in", width = 6,
       height = 6, dpi =  600, compression = "lzw")

facet <- dat_scaled |> 
      ggplot(x = mean_trophic_diversity) +
      geom_point(aes(y = comm_mean_n, x = mean_trophic_diversity, color = "mean")) +
      geom_point(aes(y = comm_sd_n, x = mean_trophic_diversity, color = "sd")) +
      geom_smooth(aes(y = comm_mean_n, x = mean_trophic_diversity, color = "mean"), 
                  method = "lm", se = FALSE) +
      geom_smooth(aes(y = comm_sd_n, x = mean_trophic_diversity, color = "sd"), 
                  method = "lm", se = FALSE) +
      facet_wrap(~Program, scales = "free") +
      labs(x = "Scaled Mean Trophic Diversity",
           y = "Scaled Temporal Mean and SD of Community Nitrogen Supply",
           color = 'measurement') +
      scale_color_manual(values = c("mean" = "black", "sd" = "darkgrey")) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            # axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold", color = "black"),
            # axis.title.y = element_blank(),
            strip.background = element_rect(color = "white"),
            strip.text = element_text(face = "bold", color = "black"),
            legend.position = "bottom",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))
ggsave("output/ms-second-round/plots/one-panel-mean-variance-BEF-faceted-sites.tiff", units = "in", width = 12,
       height = 6, dpi =  600, compression = "lzw")

mean_variance <- ggarrange(combined, facet,
                 labels = c('a)','b)'),
                 legend = 'none', common.legend = FALSE,
                 ncol = 2, nrow = 1, align = "h", hjust = 0.5)

mean_variance_2 <- annotate_figure(mean_variance,
                         bottom = text_grob(label = "Scaled Mean Trophic Diversity",
                                            just = 'centre', rot = 0,
                                            color = "black", face = "bold"),
                         left = text_grob(label = "Scaled Temporal Mean and SD of Community Nitrogen Supply",
                                          just = 'centre', rot = 90,
                                          color = "black", face = "bold"))

# ggsave("output/ms-second-round/plots/two-panel-mean-variance-BEF.tiff", units = "in", width = 12,
#        height = 6, dpi =  600, compression = "lzw")



dat1 <- dat |> 
      group_by(Program, Habitat, Site, Trophic_Group, Species) |> 
      mutate(spp_dominance = spp_mean_n/comm_mean_n) |> 
      ungroup() |> 
      group_by(Program, Habitat, Site, Trophic_Group) |> 
      mutate(troph_dominance = troph_mean_n/comm_mean_n) |> 
      ungroup()

# domb <- dat1 |> 
#       group_by(Program, Habitat, Site) |> 
#       mutate(dominant_group = max(troph_dominance),
#              weak_group = min(troph_dominance)) |> 
#       ungroup() |> 
#       ggplot(aes(x = Program, y = dominant_group, fill = Program)) +
#       geom_boxplot() +
#       labs(x = "LTER Program",
#            y = "Max Trophic Dominance of Nutrient Supply") +
#       theme_classic() +
#       theme(axis.text.x = element_text(face = "bold", color = "black"),
#             axis.text.y = element_text(face = "bold", color = "black"),
#             # axis.title.x = element_text(face = "bold", color = "black"),
#             axis.title.x = element_blank(),
#             axis.title.y = element_text(face = "bold", color = "black"),
#             # axis.title.y = element_blank(),
#             legend.position = "none",
#             legend.text = element_text(face = "bold", color = "black"),
#             legend.title = element_text(face = "bold", color = "black"))

domc <- dat1 |> 
      group_by(Program, Habitat, Site) |> 
      mutate(dominant_group = max(troph_dominance),
             weak_group = min(troph_dominance),
             difference_group = dominant_group - weak_group) |> 
      ungroup() |> 
      filter(weak_group <= 0.4) |> 
      ggplot(aes(x = Program, y = weak_group, fill = Program)) +
      geom_boxplot() +
      labs(x = "LTER Program",
           y = "Minimum Trophic Level Nutrient Supply") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold", color = "black"),
            # axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

domd <- dat1 |> 
      group_by(Program, Habitat, Site) |> 
      mutate(dominant_group = max(troph_dominance),
             weak_group = min(troph_dominance),
             difference_group = dominant_group - weak_group) |> 
      ungroup() |> 
      filter(weak_group <= 0.4) |> 
      ggplot(aes(x = Program, y = dominant_group, fill = Program)) +
      geom_boxplot() +
      labs(x = "LTER Program",
           y = "Maximum Trophic Level Nutrient Supply") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            # axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold", color = "black"),
            # axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

# doma <- dat1 |> 
#       group_by(Program, Habitat, Site) |> 
#       mutate(dominant_spp= max(spp_dominance)) |> 
#       ungroup() |> 
#       ggplot(aes(x = Program, y = dominant_spp, fill = Program)) +
#       geom_boxplot() +
#       labs(x = "LTER Program",
#            y = "Max Species Dominance of Nutrient Supply") +
#       theme_classic() +
#       theme(axis.text.x = element_text(face = "bold", color = "black"),
#             axis.text.y = element_text(face = "bold", color = "black"),
#             # axis.title.x = element_text(face = "bold", color = "black"),
#             axis.title.x = element_blank(),
#             axis.title.y = element_text(face = "bold", color = "black"),
#             # axis.title.y = element_blank(),
#             legend.position = "none",
#             legend.text = element_text(face = "bold", color = "black"),
#             legend.title = element_text(face = "bold", color = "black"))
