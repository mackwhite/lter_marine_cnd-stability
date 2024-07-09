###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Adrian Stier, Nate Lemoine
###goal(s): join cnd, community, and turnover/synchrony datasets
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")

librarian::shelf(tidyverse, googledrive, vegan, readxl, codyn, purrr)

###########################################################################
# read in csv files for joins ---------------------------------------------
###########################################################################

cnd <- read_csv("local_data/cnd_stability_site_averages_07092024.csv")
comm <- read_csv("local_data/community_data_site_averages_07092024.csv")
turnsynch <- read_csv("local_data/turnover_synchrony_site_data.csv") |> 
      separate(col = psh_vec, into = c("program", "habitat", "site"), sep = ":")

cnd_comm <- left_join(cnd, comm)
all <- left_join(cnd_comm, turnsynch)

# write_csv(all, "local_data/final_model_data_072024.csv")
