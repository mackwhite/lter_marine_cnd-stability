###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Adrian Stier, Nate Lemoine
###goal(s): join cnd, population, community, and turnover/synchrony datasets
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")

librarian::shelf(tidyverse, readr, readxl)

###########################################################################
# read in csv files for joins ---------------------------------------------
###########################################################################

cnd_pop <- read_csv('local_data/species-level-nutrient-stability_10172024.csv') |> 
      mutate(program = as.factor(program),
             habitat = as.factor(habitat),
             site = as.factor(site))

cnd_comm <- read_csv("local_data/community-level-nutrient-stability_10172024.csv") |> 
      mutate(program = as.factor(program),
             habitat = as.factor(habitat),
             site = as.factor(site))

turnsynch <- read_csv("local_data/turnover_synchrony_08012024.csv") |> 
      mutate(program = as.factor(program),
             habitat = as.factor(habitat),
             site = as.factor(site))

comm_cnd_div_mech <- left_join(cnd_comm, turnsynch, by = c("program", "habitat", "site"))

all <- left_join(comm_cnd_div_mech, cnd_pop, by = c("program", "habitat", "site"))

all_scaled <- all |> 
      select(program, scientific_name, habitat, site, everything()) |> 
      group_by(program) |> 
      ## this is a function syntax
      mutate(across(comm_mean_bm:spp_bm_stability,\(x) scale(x, center = TRUE))) |>
      ungroup()

all|> 
      rename(Program = program) |> 
      filter(spp_n_stability <= 7) |> 
      ggplot(aes(x = mean_species_richness, y = spp_n_stability, color = Program)) +
      geom_point() +  # Adds the scatter plot points
      facet_wrap(~Program, scales = 'free') + 
      geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Scaled Species Richness",
           y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))
