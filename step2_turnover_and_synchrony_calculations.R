###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): MW, AC, LK, WRJ
###goal(s): Wrangling raw COMMUNITY data such that it is ready for analysis
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, vegan, readxl, codyn, purrr)

### set google drive paths
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
      dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN.csv"))

strata_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
      dplyr::filter(name %in% c("strata_class.xlsx"))

### combine file IDs
harmonized_ids <- rbind(exc_ids, strata_ids)

### for each raw data file, download it into the consumer folder
for(k in 1:nrow(harmonized_ids)){
      
      ### download file (but silence how chatty this function is)
      googledrive::with_drive_quiet(
            googledrive::drive_download(file = harmonized_ids[k, ]$id, overwrite = T,
                                        path = file.path("tier2", harmonized_ids[k, ]$name)) )
      
      ### print success message
      message("Downloaded file ", k, " of ", nrow(harmonized_ids))
}

### cleans environment
rm(list = ls()) 

### read in clean excretion and strata data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN.csv"),stringsAsFactors = F,na.strings =".") |> 
      janitor::clean_names()
glimpse(dt)

strata_list <- readxl::read_excel(path = file.path("tier2", "strata_class.xlsx"),na=".") |> 
      ### remove decimals from numbered sites
      mutate(site = str_remove(site, "\\.0$"),
             subsite_level1 = str_remove(subsite_level1, "\\.0$"),
             subsite_level2 = str_remove(subsite_level2, "\\.0$"),
             subsite_level3 = str_remove(subsite_level3, "\\.0$"))

# set up data for summary statistics --------------------------------------

### replace NAs in subsite_level2 and subsite_level3 columns with "Not Available"
### to allow group_by function to go as far in sequence as it can for 
### each project without throwing NAs

dt1 <- dt |> 
      mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
             subsite_level2 = replace_na(subsite_level2, "Not Available"),
             subsite_level3 = replace_na(subsite_level3, "Not Available"))

### check to see NA fixes incorporated
na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column)

# look into outliers for project-species combinations -----------
### removing 'biomass buster' sharks based on requests from PISCO, SBC, and MCR 

dt_og <- dt1 |> 
      group_by(project, habitat) |> 
      ### filtering out sharks and rays that are considered "biomass busters"
      mutate(mean_dmperind = mean(dmperind_g_ind, na.rm = TRUE),  
             sd_dmperind = sd(dmperind_g_ind, na.rm = TRUE),  
             lower_bound = mean_dmperind - 5 * sd_dmperind,  
             upper_bound = mean_dmperind + 5 * sd_dmperind,
             ### +/- 5 SD cutoff... rest of sharks and rays included
             outlier = dmperind_g_ind < lower_bound | dmperind_g_ind > upper_bound,
             sharkray = grepl("\\bshark\\b|\\bray\\b", common_name, ignore.case = TRUE),
             elasmo = class %in% c("Chondrichthyes", "Elasmobranchii")) |> 
      ungroup() |> 
      filter(!(outlier & sharkray & elasmo)) |> #lose 251 sharks and rays from MCR/PISCO datasets
      dplyr::select(-lower_bound, -upper_bound, -outlier, -sharkray, -elasmo)

glimpse(dt_og)

###########################################################################
# add vertebrate and invertebrate column ~ phylum -------------------------
###########################################################################

dt_mutate <- dt_og |> 
      ### classify each individual as either being a vertebrate or invertebrate
      mutate(vert_1 = if_else(phylum == "Chordata", "vertebrate", "invertebrate")) |> 
      mutate(vert2 = if_else(is.na(vert_1) & project == "CoastalCA", "vertebrate", vert_1)) |> 
      mutate(vert = ifelse(is.na(vert2), "invertebrate", vert2)) |> 
      mutate(vertebrate_n = if_else(vert == "vertebrate" & dmperind_g_ind != 0, 1, 0),
             invertebrate_n = if_else(vert == "invertebrate" & dmperind_g_ind != 0, 1, 0)) |> 
      ### filtering out invertebrates - first manuscript focused on vertebrates (ie fishes)
      filter(vert == "vertebrate",
             ### removing invertebrate dominant projects, plus PIE given conversations with DB, NL, AS
             !project %in% c("NGA", "CCE", "PIE")) |> 
      ### removing some of the unnecessary rows      
      select(-vert, -vert_1, -vert2, -vertebrate_n, -invertebrate_n, -raw_filename, -row_num)

###########################################################################
# fixing sbc/pisco data prior to calculations ----------[08/01/2024]-------
###########################################################################

### pull out all data such that we can join with pisco and sbc later
dt_mutate_0_no_sbc_pisco <- dt_mutate |> filter(!project %in% c("CoastalCA", "SBC"))

### determine the # of individuals caught at each transect
dt_mutate_0_pisco <- dt_mutate |> filter(project == "CoastalCA") |> mutate(count = density_num_m2*60)
### filter where count is greater than zero [keep for now so you can check] and duplicate by count to make each row an individual
dt_mutate_0_pisco_1 <- dt_mutate_0_pisco |> filter(count > 0) |> expandRows(count = "count", drop = FALSE) |> 
      ### account for the step above by dividing density by count so we aren't artificially inflating the density and thus nsupply below
      mutate(density_num_m2 = density_num_m2/count)
### bind zero-data with longer, individual-per-row data
dt_mutate_0_pisco_clean <- bind_rows(dt_mutate_0_pisco |> filter(count == 0), dt_mutate_0_pisco_1) |> 
      select(-count)

### determine the # of individuals caught at each transect
dt_mutate_0_sbc <- dt_mutate |> filter(project == "SBC") |> mutate(count = density_num_m2*40)
### filter where count is greater than zero [keep for now so you can check] and duplicate by count to make each row an individual
dt_mutate_0_sbc_1 <- dt_mutate_0_sbc |> filter(count > 0) |> expandRows(count = "count", drop = FALSE) |> 
      ### account for the step above by dividing density by count so we aren't artificially inflating the density and thus nsupply below
      mutate(density_num_m2 = density_num_m2/count)
### bind zero-data with longer, individual-per-row data
dt_mutate_0_sbc_clean <- bind_rows(dt_mutate_0_sbc |> filter(count == 0), dt_mutate_0_sbc_1) |> 
      select(-count)

### join all of the datasets back together with a simple rbind
dt_mutate_05 <- rbind(dt_mutate_0_no_sbc_pisco, dt_mutate_0_pisco_clean, dt_mutate_0_sbc_clean)

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
dt_total_strata <- left_join(dt_mutate_05, 
                             strata_list1, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2")) |> 
      unite("projecthabitat", project, habitat, sep = "-", remove = FALSE) |> 
      rename(strata = ecoregion_habitat) |> 
      select(project, habitat, projecthabitat, strata, year, month, site, subsite_level1, 
             subsite_level2, subsite_level3, everything())


### Check NAs
na_count_per_column <- sapply(dt_total_strata, function(x) sum(is.na(x)))
print(na_count_per_column) #yayay

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
pisco_central <- dt_total_strata |> 
      filter(projecthabitat == "CoastalCA-ocean",
             site == "CENTRAL") |> #split pisco into central and southern
      mutate(group = subsite_level2,
             color = strata,
             units = 'm2',
             projecthabitat = "CoastalCA-ocean-CENTRAL") |> 
      ### added new resolution group wants considered for examination -> functionally the "site" for each project
      unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

pisco_south <- dt_total_strata |> 
      filter(projecthabitat == "CoastalCA-ocean",
             site == "SOUTH") |> #split pisco into central and southern
      mutate(group = subsite_level2,
             color = strata,
             units = 'm2',
             projecthabitat = "CoastalCA-ocean-SOUTH") |> 
      ### added new resolution group wants considered for examination -> functionally the "site" for each project
      unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

### FCE-estuary
fce <- dt_total_strata |> 
      filter(projecthabitat == "FCE-estuary") |>
      mutate(group = subsite_level1,
             color = strata,
             units = 'm') |> #grouped at subsite_level1
      ### added new resolution group wants considered for examination -> functionally the "site" for each project
      unite(color2, c(site, subsite_level1), sep = "-", remove = FALSE)

### MCR-ocean
mcr <- dt_total_strata |> 
      filter(projecthabitat == "MCR-ocean") |> 
      ### join site and subsite_level1 according DB request for grouping variable
      unite("group", site, subsite_level1, sep = "-", remove = FALSE) |>
      mutate(group = group,
             color = subsite_level1,
             units = 'm2') |> 
      ### added new resolution group wants considered for examination -> functionally the "site" for each project
      unite(color2, c(subsite_level1, site), sep = "-", remove = FALSE)

### SBC-ocean
sbc <- dt_total_strata |> 
      filter(projecthabitat == "SBC-ocean") |> 
      mutate(group = site,
             color = strata,
             units = 'm2') |> 
      ### added new resolution group wants considered for examination -> functionally the "site" for each project
      unite(color2, c(site, color), sep = "-", remove = FALSE)

### VCR-estuary
vcr <- dt_total_strata |> 
      filter(projecthabitat == "VCR-estuary") |> 
      mutate(group = subsite_level1,
             color = strata,
             units = 'm2') |> 
      ### added new resolution group wants considered for examination -> functionally the "site" for each project
      unite(color2, c(site, subsite_level1, color), sep = "-", remove = FALSE)

### binding everything back together, removing index row generated when saving out of R
## and arranging the data by date
dat_ready <- bind_rows(fce, mcr, pisco_central, pisco_south, sbc, vcr)

na_count_per_column <- sapply(dat_ready, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

### tidy up working environment
rm(fce, mcr, pisco_central, pisco_south, sbc, vcr)

###########################################################################
# clean up dataset names for plotting and analysis ------------------------
###########################################################################

unique(dat_ready$projecthabitat)
label_mapping <- data.frame(
      projecthabitat = unique(dat_ready$projecthabitat),
      Project = c("FCE", "MCR", "PCCC", "PCCS",
                  "SBC", "VCR")) 
print(label_mapping) #looks good

unique(dat_ready$color)
habitat_mapping <- data.frame(
      color = unique(dat_ready$color),
      Habitat = c(
            "Riverine", "Bay", #FCE
            "Fringing Reef", "Back Reef", "Fore Reef", #MCR
            "Marine Protected Area", "Reference", #PISCO-Central, PISCO-South, SBC-Beach, & SBC-Ocean
            "Seagrass", "Sand")) #VCR
print(habitat_mapping) #yayayay

dat_ready_2 <- dat_ready |> 
      left_join(label_mapping, by = "projecthabitat") |>
      left_join(habitat_mapping, by = "color") |>
      ### remove columns needed for joins up to this point
      select(-projecthabitat, -habitat, -project, -color) |> ### KEEP SITE HERE FOR SPECIES PRESENCE CALC
      ### rename columns to be more representative/clean
      ### added "_CORRECT" to maintain original designations used in CND calcs - will right when creating species-presence data frame
      rename(site_CORRECT = color2,
             program = Project, 
             habitat = Habitat) |> 
      dplyr::select(program, habitat, site, year, month, everything())

glimpse(dat_ready_2)
unique(dat_ready_2$habitat)

dat_ready_3 <- dat_ready_2 |> 
      filter(!site_CORRECT %in% c("TB-5", "RB-17", "RB-19"))
      ### keep everything below for right now
      # select(-strata, -subsite_level1, -subsite_level2, -subsite_level3, 
      #        -n_spp, -mean_size, -min_size, -group, -units)

glimpse(dat_ready_3)
unique(dat_ready_3$site_CORRECT)

###########################################################################
# turnover and synchrony metrics ------------------------------------------
###########################################################################

dat_ready_4 <- dat_ready_3 |> 
      mutate(year_month = paste(year, month, sep = "-")) |> 
      select(year_month, everything())

species_presence <- dat_ready_4 |> 
      # filter(site != "RB-17") |> #remove earlier in process
      group_by(program, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |> #remove habitat, because different than what it came in as - dont need it here
      mutate(total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
             total_bm_m2 = sum(dmperind_g_ind*density_num_m2, na.rm = TRUE),
             total_biomass = sum(total_bm_m + total_bm_m2, na.rm = TRUE),
             total_density_m = sum(density_num_m, na.rm = TRUE),
             total_density_m2 = sum(density_num_m2, na.rm = TRUE),
             total_density = sum(total_density_m + total_density_m2, na.rm = TRUE)) |> 
      ungroup() |>
      dplyr::select(
            -total_bm_m, -total_bm_m2,
            -total_density_m, -total_density_m2, -site) |> 
      rename(site = site_CORRECT) |> 
      # arrange(program, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |> 
      group_by(program, habitat, year, site, scientific_name) |> 
      summarize(mean_total_bm = mean(total_biomass),
                mean_total_dens = mean(total_density)) |> 
      ungroup()
      ### decided we did not need this for synchrony and beta calculations with
      ### with WRJ on 07/16/2024
      # mutate(mean_total_dens_1p = ifelse(
      #       mean_total_dens > 0, mean_total_dens + 1, 0), 
      #       incidence = ifelse(
      #             mean_total_dens > 0, 1, 0))

testy <- species_presence |> 
      mutate(psh = paste(program, habitat, site, sep = ":"))

psh_vec = unique(testy$psh)
df_temp <- data.frame(matrix(ncol=3,nrow=length(psh_vec)))
df_temp[,1] <- psh_vec
names(df_temp)<-c("psh_vec","beta_time","synch")

for (i in 1:length(psh_vec)){
      temp <- testy |> 
            filter(psh == psh_vec[i])
      beta_temp <- turnover(df = temp, time.var = "year", 
                            abundance.var = "mean_total_dens", 
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

# write_csv(df_temp_final, "local_data/turnover_synchrony_08012024.csv")
