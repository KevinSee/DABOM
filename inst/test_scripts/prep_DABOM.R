# Author: Kevin See
# Purpose: Test functions for preparing PTAGIS data for DABOM
# Created: 4/8/2021
# Last Modified: 5/5/2021
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

# set minimum observation date
min_obs_date = if_else(root_site %in% c("TUM", 'GRA'),
                       paste0(spwn_yr, "0301"),
                       if_else(root_site %in% c("PRA", 'PRO'),
                               paste0(spwn_yr-1, "0701"),
                               NA_character_))

# set maximum observation date
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
              min_obs_date = min_obs_date,
              max_obs_date = max_obs_date)

# determine which obs to keep
filter_ch = prepped_df %>%
  mutate(user_keep_obs = auto_keep_obs) %>%
  filter(user_keep_obs)

# # for sampling portion of tags
# n_fish = 500
# set.seed(5)
# filter_ch %<>%
#   select(tag_code) %>%
#   distinct() %>%
#   sample_n(n_fish) %>%
#   left_join(filter_ch)

fish_origin = read_csv(ptagis_file) %>%
  select(tag_code = `Tag Code`,
         origin = `Mark Rear Type Name`) %>%
  distinct() %>%
  mutate(origin = str_sub(origin, 1, 1),
         origin = recode(origin,
                         "U" = "W"))

if(root_site %in% c("GRA", "PRO")) {
  fish_origin %<>%
    mutate(origin = "W")
}


# summarize data for each tag, including origin
tag_summ = summarizeTagData(filter_ch,
                            bio_data = fish_origin)

# file path to the default and initial model
basic_modNm = paste0('~/Desktop/', root_site, '_DABOM.txt')

writeDABOM(basic_modNm,
           pc,
           configuration,
           time_varying = if_else(root_site == "GRA", T, F))

#------------------------------------------------------------------------------
# Alter default model code for species and year of
# interest; sets prior for some detection node efficiencies at 0 or 100%
# based on actual tag detection data; 0% if no tags were seen
#------------------------------------------------------------------------------

# filepath for specific JAGS model code for species and year
mod_path = paste0('~/Desktop/Test_DABOM_', root_site, '.txt')

# writes species and year specific jags code
fixNoFishNodes(init_file = basic_modNm,
               file_name = mod_path,
               filter_ch = filter_ch,
               parent_child = pc,
               configuration = configuration,
               fish_origin = fish_origin)

# Creates a function to spit out initial values for MCMC chains
init_fnc = setInitialValues(filter_ch,
                            pc,
                            configuration)

# Create all the input data for the JAGS model
jags_data = createJAGSinputs(filter_ch = filter_ch,
                             parent_child = pc,
                             configuration = configuration,
                             fish_origin = fish_origin)
if(root_site == "GRA") {
  jags_data = c(jags_data,
                addTimeVaryData(filter_ch,
                                start_date = paste0(spwn_yr, "0301"),
                                end_date = paste0(spwn_yr, "0817")))

  time_strata = STADEM::weeklyStrata(start_date = paste0(spwn_yr, "0301"),
                                     end_date = paste0(spwn_yr, "0817"))
}

# Tell JAGS which parameters in the model that it should save.
jags_params = setSavedParams(model_file = mod_path,
                             time_varying = if_else(root_site == "GRA", T, F))


# test running the model
jags = jags.model(mod_path,
                  data = jags_data,
                  inits = init_fnc,
                  n.chains = 1,
                  n.adapt = 5)


#--------------------------------------
# test the MCMC outcome and summary functions
dabom_mod = coda.samples(jags,
                         jags_params,
                         n.iter = 10)


detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                   filter_ch = filter_ch) %>%
  filter(!is.na(mean))

# compile all movement probabilities, and multiply them appropriately
# compile all movement probabilities, and multiply them appropriately
if(root_site == "PRO") {
  trans_df = compileTransProbs_PRO(dabom_mod,
                                   pc)

  tot_escp = tibble(origin = c(1,2),
                    tot_escp = c(1132, 0))

} else if(root_site == "TUM") {
  trans_df = compileTransProbs_TUM(dabom_mod,
                                   pc)

  tot_escp = tibble(origin = c(1,2),
                    tot_escp = c(1086, 1378))


} else if(root_site == "PRA") {
  trans_df = compileTransProbs_PRA(dabom_mod,
                                   pc)

} else if(root_site == "GRA") {
  # trans_df = compileTransProbs_GRA(dabom_mod,
  #                                  pc)

  # run STADEM model to get weekly escapement estimates
  # of wild fish at Lower Granite
  trap_db = system.file("extdata",
                       "Chnk2014_TrapDatabase.csv",
                       package = "PITcleanr",
                       mustWork = TRUE) %>%
    STADEM::readLGRtrapDB()

  stadem_data = STADEM::compileGRAdata(yr = spwn_yr,
                                       spp = ptagis_file %>%
                                         str_split("/") %>%
                                         unlist() %>%
                                         last() %>%
                                         str_split("_") %>%
                                         unlist() %>%
                                         magrittr::extract(2),
                                       dam = "LWG",
                                       start_date = paste0(spwn_yr, "0301"),
                                       end_date = paste0(spwn_yr, "0817"),
                                       damPIT = "GRA",
                                       trap_dbase = trap_db)

  # compile everything into a list to pass to JAGS
  jags_data_list = STADEM::prepJAGS(stadem_data[['weeklyData']])

  model_file_nm = '~/Desktop/STADEM_JAGS_model.txt'

  stadem_mod = STADEM::runSTADEMmodel(file_name = model_file_nm,
                                      mcmc_chainLength = 40000,
                                      mcmc_burn = 10000,
                                      mcmc_thin = 30,
                                      mcmc_chains = 4,
                                      jags_data = jags_data_list,
                                      seed = 5,
                                      weekly_params = T,
                                      win_model = "neg_bin",
                                      trap_est = T)


  # combine weekly escapement with other transition probabilities
  escp_summ = calcTribEscape_GRA(dabom_mod,
                               stadem_mod,
                               parent_child = pc,
                               summ_results = T)

  # get estimates for reporting groups
  rep_grp = calcRepGrpEscape_GRA(dabom_mod,
                       stadem_mod,
                       parent_child = pc,
                       spp = "Chinook")

}

if(root_site %in% c("PRO", 'TUM')) {
  trans_df %>%
    left_join(tot_escp,
              by = "origin") %>%
    mutate(escp = tot_escp * value) %>%
    group_by(location = param,
             origin) %>%
    summarise(mean = mean(escp),
              median = median(escp),
              mode = estMode(escp),
              sd = sd(escp),
              skew = moments::skewness(escp),
              kurtosis = moments::kurtosis(escp),
              lowerCI = coda::HPDinterval(coda::as.mcmc(escp))[,1],
              upperCI = coda::HPDinterval(coda::as.mcmc(escp))[,2],
              .groups = "drop") %>%
    mutate(across(c(mean, median, mode, sd, matches('CI$')),
                  ~ if_else(. < 0, 0, .))) %>%
    mutate(across(c(mean, median, mode, sd, skew, kurtosis, matches('CI$')),
                  round,
                  digits = 2)) %>%
    arrange(desc(origin), location) %>%
    as.data.frame()
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
