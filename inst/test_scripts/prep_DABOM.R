# Author: Kevin See
# Purpose: Test functions for preparing PTAGIS data for DABOM
# Created: 4/8/2021
# Last Modified: 4/27/2021
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(PITcleanr)
# library(DABOM)
library(rjags)
devtools::load_all()

#-----------------------------------------------------------------
# pick a DABOM version to test
root_site = c("GRA", 'PRA', "TUM", "PRO")[3]

# determine where various test files are located
ptagis_file_list = list(GRA = "LGR_Chinook_2014.csv",
                        PRA = "UC_Sthd_2015.csv",
                        TUM = "TUM_Chinook_2015.csv",
                        PRO = "PRO_Steelhead_2019.csv")

ptagis_file = system.file("extdata",
                          ptagis_file_list[[root_site]],
                          package = "PITcleanr",
                          mustWork = TRUE)

parent_child_file = system.file("extdata",
                                paste0("parent_child_", root_site, ".csv"),
                                package = "PITcleanr",
                                mustWork = TRUE)

config_file = system.file("extdata",
                          paste0("configuration_", root_site, ".csv"),
                          package = "PITcleanr",
                          mustWork = TRUE)

# determine maximum allowed observed detection date
spwn_yr = ptagis_file %>%
  str_split("/") %>%
  unlist() %>%
  last() %>%
  str_extract("[:digit:]+") %>%
  as.numeric()

max_obs_date = if_else(root_site %in% c("TUM", 'GRA'),
                       paste0(spwn_yr, "0930"),
                       if_else(root_site %in% c("PRA", 'PRO'),
                               paste0(spwn_yr, "0630"),
                               NA_character_))

# read in parent-child table
pc = read_csv(parent_child_file)
# read in configuration file
configuration = read_csv(config_file)

# add nodes to parent-child table
pc_nodes = addParentChildNodes(pc,
                               configuration = configuration)

# prep data for DABOM
prepped_df = compress(ptagis_file,
                      configuration = configuration) %>%
  prepWrapper(parent_child = pc_nodes,
              start_node = root_site,
              max_obs_date = max_obs_date)

# determine which obs to keep
filter_ch = prepped_df %>%
  mutate(user_keep_obs = auto_keep_obs) %>%
  filter(user_keep_obs)

tag_summ = summarizeTagData(filter_ch,
                            bio_data = read_csv(ptagis_file) %>%
                              select(tag_code = `Tag Code`,
                                     species = `Mark Species Name`,
                                     origin = `Mark Rear Type Name`) %>%
                              distinct()) %>%
  mutate(origin = str_sub(origin, 1, 1),
         origin = recode(origin,
                         "U" = "W"))

# for Prosser, mark all fish as wild
if(root_site == "PRO") {
  tag_summ %<>%
    mutate(origin = "W")
}

spawn_site = estimateSpawnLoc(filter_ch, spawn_site = T, ptagis_file) %>%
  select(-event_type_name) %>%
  distinct() %>%
  left_join(pc %>%
              buildNodeOrder() %>%
              select(spawn_site = node,
                     spawn_path = path))

# # construct DABOM matrices
# dabom_df = createDABOMcapHist(filter_ch,
#                               parent_child = pc,
#                               configuration = configuration,
#                               split_matrices = F)
#
# dabom_list = createDABOMcapHist(filter_ch,
#                                 parent_child = pc,
#                                 configuration = configuration,
#                                 split_matrices = T)
#
# if(root_site %in% c("PRA", 'TUM')) {
#   dabom_list$fishOrigin = dabom_df %>%
#     left_join(tag_summ %>%
#                 select(tag_code, origin) %>%
#                 distinct() %>%
#                 mutate(origin = recode(origin,
#                                        "W" = 1,
#                                        "H" = 2))) %>%
#     pull(origin)
# } else if(root_site == "PRO") {
#   dabom_list$fishOrigin = rep(1, nrow(dabom_df))
# }

# file path to the default and initial model
basic_modNm = paste0('~/Desktop/', root_site, '_DABOM.txt')

# if(root_site == "GRA") {
#   writeDABOM_LGD(basic_modNm)
# } else if(root_site == "PRA") {
#   writeDABOM_PRA(basic_modNm)
# } else if(root_site == "TUM") {
#   writeDABOM_TUM(basic_modNm)
# } else if(root_site == "PRO") {
#   writeDABOM_PRO(basic_modNm)
# }

writeDABOM(basic_modNm,
           pc,
           configuration)

#------------------------------------------------------------------------------
# Alter default model code for species and year of
# interest; sets prior for some detection node efficiencies at 0 or 100%
# based on actual tag detection data; 0% if no tags were seen
#------------------------------------------------------------------------------

# filepath for specific JAGS model code for species and year
mod_path = paste0('~/Desktop/Test_DABOM_', root_site, '.txt')

# writes species and year specific jags code
fixNoFishNodes(basic_modNm,
               mod_path,
               filter_ch = filter_ch,
               parent_child = pc,
               configuration = configuration)

# # Creates a function to spit out initial values for MCMC chains
# if(root_site == "GRA") {
#   init_fnc = setInitialValues_LGD(dabom_list)
# } else if(root_site == "PRA") {
#   init_fnc = setInitialValues_PRA(dabom_list)
# } else if(root_site == "TUM") {
#   init_fnc = setInitialValues_TUM(dabom_list,
#                                   mod_path,
#                                   pc)
# } else if(root_site == "PRO") {
#   init_fnc = setInitialValues_PRO(dabom_list,
#                                   mod_path,
#                                   pc)
# }
#
# # Create all the input data for the JAGS model
# if(root_site == "GRA") {
#   jags_data = c(createJAGSinputs_LGD(dabom_list),
#                 addTimeVaryData(filter_ch))
# } else if(root_site == "PRA") {
#   jags_data = createJAGSinputs_PRA(dabom_list)
# } else if(root_site == "TUM") {
#   jags_data = createJAGSinputs_TUM(dabom_list,
#                                    mod_path,
#                                    pc)
# } else if(root_site == "PRO") {
#   jags_data = createJAGSinputs_PRO(dabom_list,
#                                    mod_path,
#                                    pc)
# }

# Creates a function to spit out initial values for MCMC chains
init_fnc = setInitialValues(filter_ch,
                            pc,
                            configuration)

# Create all the input data for the JAGS model
jags_data = createJAGSinputs(filter_ch = filter_ch,
                             parent_child = pc,
                             configuration = configuration,
                             fish_origin = tag_summ %>%
                               select(tag_code, origin) %>%
                               distinct())


# Tell JAGS which parameters in the model that it should save.
jags_params = setSavedParams(model_file = mod_path,
                             time_varying = if_else(root_site == "GRA", T, F))


# test running the model
jags = jags.model(mod_path,
                  data = jags_data,
                  inits = init_fnc,
                  n.chains = 1,
                  n.adapt = 10)


#--------------------------------------
# test the MCMC outcome and summary functions
dabom_mod = coda.samples(jags,
                         jags_params,
                         n.iter = 10)


detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                   filter_ch = filter_ch) %>%
  filter(!is.na(mean))

# compile all movement probabilities, and multiply them appropriately
if(root_site == "PRO") {
  trans_df = compileTransProbs_PRO(dabom_mod,
                                   pc)
} else if(root_site == "TUM") {
  trans_df = compileTransProbs_TUM(dabom_mod,
                                   pc)
}



#--------------------------------------
# issues
fish_num = 3288
dabom_df %>%
  slice(fish_num) %>%
  pivot_longer(-c(tag_code:start_date),
               names_to = "node",
               values_to = "seen") %>%
  filter(seen == 1)


tmp = init_fnc()
tmp$a_OKL[fish_num]
tmp$a_LMR[fish_num]
tmp$z_meth[fish_num]
tmp$a_MRC[fish_num]

names(dabom_list$Methow)

sites_df = writeOldNetworks()$PriestRapids
sites_df %>%
  filter(SiteID %in% c("MRW", 'METH'))
