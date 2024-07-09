
### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, vegan, readxl, codyn, purrr)

### read in clean excretion and strata data from google drive
filter_dt <-read_csv("local_data/filtered_dataset.csv")

strata_list <- readxl::read_excel(path = file.path("tier2", "strata_class.xlsx"),na=".") |> 
  ### remove decimals from numbered sites
  mutate(site = str_remove(site, "\\.0$"),
         subsite_level1 = str_remove(subsite_level1, "\\.0$"),
         subsite_level2 = str_remove(subsite_level2, "\\.0$"),
         subsite_level3 = str_remove(subsite_level3, "\\.0$"))

###########################################################################
# add strata of interest to each project ----------------------------------
###########################################################################

### set up strata_list such that the "Not Available" aligns with dt_total
strata_list1 <- strata_list %>%
      mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
             subsite_level2 = replace_na(subsite_level2, "Not Available"),
             subsite_level3 = replace_na(subsite_level3, "Not Available")) |> 
      ### LK suggested to remove subsite_level 3 given updates to PISCO 
      select(-subsite_level3) |> 
      distinct()

### join together the datasets of nutrient supply and biomass with strata
dt_total_strata <- left_join(filter_dt, 
                             strata_list1, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2")) |> 
      ### special case in which "site" column depicts strata for some programs 
      mutate(strata = if_else(is.na(ecoregion_habitat), site, ecoregion_habitat)) |> 
      dplyr::select(-ecoregion_habitat)

### Check NAs
na_count_per_column <- sapply(dt_total_strata, function(x) sum(is.na(x)))
print(na_count_per_column) #yayay

###########################################################################
# generate pseudo date column for each project ----------------------------
###########################################################################

dt_total_strata_date <- dt_total_strata |>
      ### create project-habitat column since some projects sample multiple habitats (i.e., SBC ocean & beach)
      unite("projecthabitat", project, habitat, sep = "-", remove = FALSE)
# ### create date columns for timeseries plotting - do not need this, 
# ### unless we want to generate time series plot and we can do that later
# mutate(sdate = ymd(paste(year, month, "01", sep = "-"))) 

###########################################################################
# set up individual projects/habitats for analyses and plotting -----------
###########################################################################

### Below I have separated each unique projecthabitat out to mutate new columns based on either
# the strata they want their data colored by (i.e., color = XXXX)and the level to which they want
# their data summarized (i.e., for FCE-estuary, I want summarized at subsite_level1..
# whereas SBC wants their data summarized at the site level. This approach sets up
# an easy way to map plots across all unique projecthabitats, instead of doing them
# individually

### CoastalCA-ocean
pisco_central <- dt_total_strata_date |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "CENTRAL") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-CENTRAL") |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

pisco_south <- dt_total_strata_date |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "SOUTH") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-SOUTH") |> 
  group_by(subsite_level2, year) |> 
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

### FCE-estuary
fce <- dt_total_strata_date |> 
  filter(projecthabitat == "FCE-estuary") |>
  mutate(group = subsite_level1,
         color = strata,
         units = 'm') |> #grouped at subsite_level1
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(site, subsite_level1), sep = "-", remove = FALSE)
### reverts back to hydrologic year to make more sense of dataset - data is collected across calendar years but considered sequential (i.e., November - June)
### this was done in step5 in June 2024 - so copy below line out, FCE needs to be set up by "hydro year"
# mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

### MCR-ocean
mcr <- dt_total_strata_date |> 
  filter(projecthabitat == "MCR-ocean") |> 
  ### join site and subsite_level1 according DB request for grouping variable
  unite("group", site, subsite_level1, sep = "-", remove = FALSE) |>
  mutate(group = group,
         color = subsite_level1,
         units = 'm2') |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level1, site), sep = "-", remove = FALSE)

### SBC-ocean
sbc_reef <- dt_total_strata_date |> 
  filter(projecthabitat == "SBC-ocean") |> 
  mutate(group = site,
         color = strata,
         units = 'm2') |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(site, color), sep = "-", remove = FALSE)

### VCR-estuary
vcr <- dt_total_strata_date |> 
  filter(projecthabitat == "VCR-estuary") |> 
  mutate(group = subsite_level1,
         color = strata,
         units = 'm2') |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level1, color), sep = "-", remove = FALSE)

### binding everything back together, removing index row generated when saving out of R
## and arranging the data by date
dat_ready <- bind_rows(fce, mcr, pisco_central, pisco_south, sbc_reef, vcr)

na_count_per_column <- sapply(dat_ready, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

### tidy up working environment
rm(fce, mcr, pisco_central, pisco_south, sbc_reef, vcr)

###########################################################################
# clean up dataset names for plotting and analysis ------------------------
###########################################################################

unique(dat_ready$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dat_ready$projecthabitat),
  Project = c("FCE", "MCR", "PCCC", "PCCS",
              "SBCO", "VCR")) 
print(label_mapping) #looks good

unique(dat_ready$color)
habitat_mapping <- data.frame(
  color = unique(dat_ready$color),
  Habitat = c(
    "Riverine", "Bay", #FCE
    "Fringing Reef", "Back Reef", "Fore Reef", #MCR
    "Marine Protected Area", "Reference", #PISCO-Central, PISCO-South, & SBC-Ocean
    "Seagrass", "Sand")) #VCR
print(habitat_mapping) #yayayay

dat_ready_2 <- dat_ready |> 
  select(-date) |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  ### remove columns needed for joins up to this point
  select(-projecthabitat, -habitat, -project, -color, -site) |> 
  ### rename columns to be more representative/clean
  rename(site = color2,
         program = Project, 
         habitat = Habitat) |> 
  dplyr::select(program, habitat, year, month, vert, everything())

na_count_per_column <- sapply(dat_ready_2, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

unique(dat_ready_2$habitat)
unique(dat_ready_2$site)

dat_ready_3 <- dat_ready_2 |> 
  filter(site != "RB-17")

glimpse(dat_ready_3)
unique(dat_ready_3$site)
# write_csv(dat_ready_3, "local_data/community_data_filtered_07092024.csv")

na_count_per_column <- sapply(dat_ready_3, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

step_1_div <- dat_ready_3 |>
  filter(dmperind_g_ind != 0) |>
  group_by(program, habitat, year, month, site) |> 
  summarize(
    Species_Richness = length(unique(scientific_name)),
    Species_Shannon_Diversity_Index = diversity(x = table(scientific_name), index = "shannon"),
    Species_Inverse_Simpson_Diversity_Index = diversity(x = table(scientific_name), index = "invsimpson"),
    Trophic_Shannon_Diversity_Index = diversity(x = table(diet_cat), index = "shannon"),
    Trophic_Inverse_Simpson_Diversity_Index = diversity(x = table(diet_cat), index = "invsimpson")
    # Fisher_Alpha_Div = fisher.alpha(table(scientific_name))
  ) |>
  ungroup()

na_count_per_column <- sapply(step_1_div, function(x) sum(is.na(x)))
print(na_count_per_column)

step_1_summary <- step_1_div |> 
      group_by(program, habitat, site) |> 
      summarise(mean_spp_rich = mean(Species_Richness),
                cv_spp_rich = (sd(Species_Richness, na.rm = TRUE) / mean(Species_Richness, na.rm = TRUE)),
                spp_rich_stability = 1/cv_spp_rich,
                mean_SppInvSimpDivInd = mean(Species_Inverse_Simpson_Diversity_Index),
                cv_SppInvSimpDivInd = (sd(Species_Inverse_Simpson_Diversity_Index, na.rm = TRUE) / mean(Species_Inverse_Simpson_Diversity_Index, na.rm = TRUE)),
                SppInvSimpDivInd_stability = 1/cv_SppInvSimpDivInd,
                mean_TrophInvSimpDivInd = mean(Trophic_Inverse_Simpson_Diversity_Index),
                cv_TrophInvSimpDivInd = (sd(Trophic_Inverse_Simpson_Diversity_Index, na.rm = TRUE) / mean(Trophic_Inverse_Simpson_Diversity_Index, na.rm = TRUE)),
                TrophInvSimpDivInd_stability = 1/cv_TrophInvSimpDivInd)|> 
      ### omit one site with NA here - it appears because there was no replication of the sites (i.e., one-offs in datasets)
      na.omit() |> 
      ungroup()

###########################################################################
# write this out so it aligns with what we have from cnd data wran --------
# write_csv(step_1_summary, "local_data/community_data_site_averages_07092024.csv")
###########################################################################

###########################################################################
# turnover and synchrony metrics ------------------------------------------
###########################################################################

dat_ready_4 <- dat_ready_3 |> 
      mutate(year_month = paste(year, month, sep = "-")) |> 
      select(year_month, everything())

species_presence <- dat_ready_4 |> 
      # filter(site != "RB-17") |> #remove earlier in process
      group_by(program, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |> 
      mutate(total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
             total_bm_m2 = sum(dmperind_g_ind*density_num_m2, na.rm = TRUE),
             total_bm_m3 = sum(dmperind_g_ind*density_num_m3, na.rm = TRUE),
             total_biomass = sum(total_bm_m + total_bm_m2 + total_bm_m3, na.rm = TRUE),
             total_density_m = sum(density_num_m, na.rm = TRUE),
             total_density_m2 = sum(density_num_m2, na.rm = TRUE),
             total_density_m3 = sum(density_num_m3, na.rm = TRUE),
             total_density = sum(total_density_m + total_density_m2 + total_density_m3, na.rm = TRUE)) |> 
      ungroup() |>
      dplyr::select(
            -total_bm_m, -total_bm_m2, -total_bm_m3) |> 
      arrange(program, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |> 
      group_by(program, habitat, year, site, scientific_name) |> 
      summarize(mean_total_bm = mean(total_biomass),
                mean_total_dens = mean(total_density)) |> 
      ungroup() |> 
      mutate(mean_total_dens_1p = ifelse(
            mean_total_dens > 0, mean_total_dens + 1, 0), 
            incidence = ifelse(
                  mean_total_dens > 0, 1, 0))

testy <- species_presence |> 
      mutate(psh = paste(program, habitat, site, sep = ":")) |> 
      filter(site != "TB-5")

psh_vec = unique(testy$psh)
df_temp <- data.frame(matrix(ncol=3,nrow=length(psh_vec)))
df_temp[,1] <- psh_vec
names(df_temp)<-c("psh_vec","beta_time","synch")

for (i in 1:length(psh_vec)){
      temp <- testy |> 
            filter(psh == psh_vec[i])
      beta_temp <- turnover(df = temp, time.var = "year", 
                            abundance.var = "mean_total_dens_1p", 
                            species.var = "scientific_name",
                            metric = "total")
      
      df_temp[i,2]<-mean(beta_temp[,1])
      
      
      synch_temp <- synchrony (df=temp,
                               time.var="year",
                               species.var="scientific_name",
                               abundance.var ="mean_total_bm",
                               metric = "Loreau",
                               replicate.var=NA)
      df_temp[i,3]<-synch_temp
}

df_temp_final <- df_temp |> 
      separate(col = psh_vec, into = c("program", "habitat", "site"), sep = ":")

# write_csv(df_temp, "local_data/turnover_synchrony_site_data.csv")
