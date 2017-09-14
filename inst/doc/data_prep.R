## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(eval=FALSE, echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
#  install.packages('devtools')
#  devtools::install_github("ryankinzer/DABOM")

## ------------------------------------------------------------------------
#  library(DABOM)
#  library(dplyr)

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # load trap data
#  #------------------------------------------------------------------------------
#  View(chinook15_trapdata)
#  # chinook15_trapdata <- LGTrapData(filename = 'dbase_path',
#  #                       species = 'Chinook',
#  #                       spawnyear = '2015')

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # select valid tag records only
#  # if you don't have access to the database, an example data set is installed
#  # with the package
#  # you could load RO's original tag list, but you would also need to change
#  # a couple field names.
#  #------------------------------------------------------------------------------
#  valid_tags <- validTags(trapdata = chinook15_trapdata)

## ------------------------------------------------------------------------
#  tag_codes <- valid_tags$TagID
#  # write.table(tag_codes, file = '.../tag_codes.txt',quote = FALSE, sep = '\t',
#  #               row.names = FALSE, col.names = FALSE)

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # load tag observation file
#  #------------------------------------------------------------------------------
#  View(chinook15_obs)
#  # Examples to read in the observation data
#  # chinook15_obs <- readxl::read_excel('filepath')
#  # chinook15_obs <- readr::read_csv('filepath')

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # load configuration and parent-child files
#  #------------------------------------------------------------------------------
#  View(config)
#  View(parentchild)

## ---- message=FALSE, warning=FALSE---------------------------------------
#  valid_obs_dat <- nodeAssign(valid_tags = valid_tags, observation = chinook15_obs,
#                             configuration = config, truncate = TRUE)

## ---- message=FALSE, warning=FALSE---------------------------------------
#  #------------------------------------------------------------------------------
#  # create a data frame with all valid paths identified in the parent child table
#  valid_paths <- validPaths(parent_child = parentchild)

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # Greg's original process and logic
#  #------------------------------------------------------------------------------
#  fish_obs <- fishPaths(valid_obs_dat, valid_paths)
#  #
#  # fish_obs %>%
#  #   filter(UserProcStatus == '') %>%
#  #   distinct(TagID) %>%
#  #   summarise(n())

## ------------------------------------------------------------------------
#  # figure out if fish movement is upstream or downstream, and in the same path
#  # then truncate to only model observations
#  # RK and Rick thought process.
#  fish_obs2 <- spawnerPaths(valid_obs_dat, valid_paths)
#  
#  # fish_obs2 %>%
#  #     filter(is.na(Direction)) %>%
#  #     distinct(TagID) %>%
#  #     nrow()

## ------------------------------------------------------------------------
#  write.csv(fish_obs, file = './data/Data_Output/fishPaths_fnc_output.csv')
#  write.csv(fish_obs2, file = './data/Data_Output/spawnerPaths_fnc_output.csv')

