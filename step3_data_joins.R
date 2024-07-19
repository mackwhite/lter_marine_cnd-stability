###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Adrian Stier, Nate Lemoine
###goal(s): join cnd, community, and turnover/synchrony datasets
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")

librarian::shelf(tidyverse, readr, readxl)

###########################################################################
# read in csv files for joins ---------------------------------------------
###########################################################################

cnd_comm <- read_csv("local_data/cnd_mdl_data_07192024.csv") |> 
      mutate(program = as.factor(program),
             habitat = as.factor(habitat),
             site = as.factor(site))

turnsynch <- read_csv("local_data/turnover_synchrony_07192024.csv") |> 
      mutate(program = as.factor(program),
             habitat = as.factor(habitat),
             site = as.factor(site))

all <- left_join(cnd_comm, turnsynch, by = c("program", "habitat", "site"))
glimpse(all)

# write_csv(all, "local_data/final_model_data_07192024.csv") #AC says YAY
