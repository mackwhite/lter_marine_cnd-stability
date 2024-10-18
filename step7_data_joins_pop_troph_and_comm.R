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

cnd_pop <- read_csv('local_data/species-level-nutrient-stability_10182024.csv') |> 
      mutate(program = as.factor(program),
             habitat = as.factor(habitat),
             site = as.factor(site))

cnd_troph <- read_csv('local_data/trophic-level-nutrient-stability_10172024.csv') |> 
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

all <- left_join(comm_cnd_div_mech, cnd_troph, by = c("program", "habitat", "site"))

df_raw <- left_join(all, cnd_pop, by = c("program", "habitat", "site", "troph_group"))
# write_csv(df_raw, "local_data/dsr-eco-org-raw.csv")

df_scaled <- df_raw |> 
      select(program, troph_group, scientific_name, habitat, site, everything()) |> 
      group_by(program) |> 
      ## this is a function syntax
      mutate(across(comm_mean_bm:spp_bm_stability,\(x) scale(x, center = TRUE))) |>
      ungroup()

# write_csv(df_scaled, "local_data/dsr-eco-org-scaled.csv")