# Author: Kevin See
# Purpose: Test functions for preparing PTAGIS data for DABOM
# Created: 4/8/2021
# Last Modified: 2/21/2024
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(usethis)
library(PITcleanr)
# library(DABOM)
library(rjags)
devtools::load_all()

#-----------------------------------------------------------------
# pick a DABOM version to test
root_site = c("LGR", 'PRA', "TUM", "PRO")[1]

# determine where various test files are located
ptagis_file_list = list(LGR = "LGR_chnk_cth_2019.csv",
                        PRA = "PRA_sthd_cth_2018.csv",
                        TUM = "TUM_chnk_cth_2018.csv",
                        PRO = "PRO_sthd_cth_2012.csv")

ptagis_file = system.file("extdata",
                          ptagis_file_list[[root_site]],
                          package = "PITcleanr",
                          mustWork = TRUE)

parent_child_file = system.file("extdata",
                                paste0(root_site, "_parent_child.csv"),
                                package = "PITcleanr",
                                mustWork = TRUE)

config_file = system.file("extdata",
                          paste0(root_site, "_configuration.csv"),
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
min_obs_date = if_else(root_site %in% c("TUM", 'LGR'),
                       paste0(spwn_yr, "0301"),
                       if_else(root_site %in% c("PRA", 'PRO'),
                               paste0(spwn_yr-1, "0701"),
                               NA_character_))

# set maximum observation date
max_obs_date = if_else(root_site %in% c("TUM", 'LGR'),
                       paste0(spwn_yr, "0930"),
                       if_else(root_site %in% c("PRA", 'PRO'),
                               paste0(spwn_yr, "0630"),
                               NA_character_))

# read in parent-child table
pc = read_csv(parent_child_file,
              show_col_types = F)
# read in configuration file
configuration = read_csv(config_file,
                         show_col_types = F)

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

if(root_site %in% c("LGR", "PRO")) {
  fish_origin %<>%
    mutate(origin = "W")
}


# summarize data for each tag, including origin
tag_summ = summarizeTagData(filter_ch,
                            bio_data = fish_origin)

# file path to the default and initial model
# desktop_path = file.path(dirname(path.expand('~')),'Desktop')
desktop_path = file.path("O:Desktop")


basic_modNm = file.path(desktop_path,
                        paste0(root_site, "_DABOM.txt"))

writeDABOM(basic_modNm,
           pc,
           configuration,
           time_varying = if_else(root_site == "LGR", T, F))

#------------------------------------------------------------------------------
# Alter default model code for species and year of
# interest; sets prior for some detection node efficiencies at 0 or 100%
# based on actual tag detection data; 0% if no tags were seen
#------------------------------------------------------------------------------

# filepath for specific JAGS model code for species and year
mod_path = file.path(desktop_path,
                     paste0('DABOM_', root_site, '_Test.txt'))

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
if(root_site == "LGR") {
  jags_data = c(jags_data,
                addTimeVaryData(filter_ch,
                                start_date = paste0(spwn_yr, "0301"),
                                end_date = paste0(spwn_yr, "0817")))

  time_strata = STADEM::weeklyStrata(start_date = paste0(spwn_yr, "0301"),
                                     end_date = paste0(spwn_yr, "0817"))
}

# Tell JAGS which parameters in the model that it should save.
jags_params = setSavedParams(model_file = mod_path,
                             time_varying = if_else(root_site == "LGR", T, F))


# test running the model
jags = jags.model(mod_path,
                  data = jags_data,
                  inits = init_fnc,
                  # n.chains = 1,
                  # n.adapt = 5)
                  n.chains = 4,
                  n.adapt = 5000)


#--------------------------------------
# test the MCMC outcome and summary functions
dabom_mod = coda.samples(jags,
                         jags_params,
                         # n.iter = 10)
                         n.iter = 5000,
                         thin = 10)


if(root_site == "TUM") {
  dabom_samp_tum = dabom_mod
  usethis::use_data(dabom_samp_tum,
                    version = 3,
                    overwrite = T)
}

if(root_site == "LGR") {
  dabom_samp_lgr = dabom_mod
  usethis::use_data(dabom_samp_lgr,
                    version = 3,
                    overwrite = T)
}


detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                   filter_ch = filter_ch) %>%
  filter(!is.na(mean))

# compile all movement probabilities, and multiply them appropriately
trans_post <-
  extractTransPost(dabom_mod = dabom_mod,
                   parent_child = pc,
                   configuration = configuration)

if(root_site %in% c("PRO", "PRA", 'TUM')) {
  trans_df <-
    compileTransProbs(trans_post,
                      parent_child = pc,
                      time_vary_only = FALSE)
}


if(root_site == "PRO") {
  tot_escp = tibble(origin = c(1,2),
                    tot_escp = c(1132, 0))

} else if(root_site == "TUM") {
  tot_escp = tibble(origin = c(1,2),
                    tot_escp = c(1086, 1378))

} else if(root_site == "PRA") {
  # tot_escp = tibble(origin = c(1,2),
  #                   tot_escp = c(1086, 1378))


} else if(root_site == "LGR") {

  main_post <-
    compileTransProbs(trans_post,
                      parent_child = pc,
                      time_vary_only = T) |>
    filter(origin == 1)

  # main_summ <-
  #   summarisePost(main_post,
  #                 value,
  #                 param,
  #                 strata_num,
  #                 origin)
  #
  # main_summ |>
  #   filter(origin == 1,
  #          median > 0) |>
  #   ggplot(aes(x = strata_num,
  #              y = median,
  #              color = param,
  #              fill = param)) +
  #   geom_ribbon(aes(ymin = lower_ci,
  #                   ymax = upper_ci),
  #               color = NA,
  #               alpha = 0.2) +
  #   geom_line() +
  #   theme_bw()

  # get example STADEM data and MCMC results from STADEM package
  library(STADEM)
  data("stadem_mod")

  # posteriors of STADEM abundance by strata_num
  abund_post <-
    STADEM::extractPost(stadem_mod,
                        param_nms = c("X.new.wild",
                                      "X.new.hatch")) |>
    mutate(origin = case_when(str_detect(param, "wild") ~ 1,
                              str_detect(param, "hatch") ~ 2,
                              .default = NA_real_)) |>
    rename(abund_param = param) |>
    filter(origin == 1)

  # escapement to each main branch across whole season
  main_escp <-
    calcAbundPost(move_post = main_post,
                  abund_post = abund_post,
                  time_vary_param_nm = "strata_num")

  summarisePost(main_escp,
                abund,
                param,
                origin) |>
    filter(mean > 0)


  # compile tributary branch movement parameters
  branch_post <-
    compileTransProbs(trans_post,
                      parent_child = pc,
                      time_vary_only = F)

  # estimate escapement to all tributary sites
  trib_escp <-
    calcAbundPost(branch_post,
                  main_escp |>
                    rename(main_branch = param,
                           tot_abund = abund),
                  .move_vars = c("origin",
                                 "main_branch",
                                 "param"),
                  .abund_vars = c("origin",
                                  "main_branch"))

  escp_summ <-
    summarisePost(trib_escp,
                  abund,
                  main_branch,
                  param,
                  origin)

}

if(root_site %in% c("PRO", 'TUM')) {
  trans_df %>%
    left_join(tot_escp,
              by = "origin") %>%
    mutate(escp = tot_escp * value) %>%
    summarisePost(escp,
                  location = param,
                  origin) |>
    mutate(across(c(mean, median, mode, sd, skew, kurtosis, matches('CI$')),
                  ~ round(.,
                          digits = 2))) %>%
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
