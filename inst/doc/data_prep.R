## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(eval=FALSE, echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
#  install.packages('devtools')
#  devtools::install_github("ryankinzer/DABOM")

## ------------------------------------------------------------------------
#  library(DABOM)
#  library(dplyr)
#  library(readxl)

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # load trap data
#  #------------------------------------------------------------------------------
#  trap_df <- LGTrapData(filename = '../data/Data_Input/LGTrappingExportJodyW_2015_Example.accdb',
#                        species = 'Chinook',
#                        spawnyear = '2015')

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # select valid tag records only
#  # if you don't have access to the database, uncomment the read_excel line
#  # and load the example dataset from Rick O.
#  #------------------------------------------------------------------------------
#  valid_tags <- validTags(trapdata = trap_df)
#  # missing one fish from RO's example file: 2015 Chinook tag list ro 9-6-17.xlsx
#  # valid_tags <- readxl::read_excel('../data/Data_Input/2015 Chinook tag list ro 9-6-17.xlsx')

## ------------------------------------------------------------------------
#  tag_codes <- valid_tags$TagID
#  write.table(tag_codes, file = '../data/Data_Output/tag_codes.txt',quote = FALSE, sep = '\t',
#                row.names = FALSE, col.names = FALSE)

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # load tag observation file
#  #------------------------------------------------------------------------------
#  obs <- readxl::read_excel('../data/Data_Input/Complete Tag History.xlsx')
#  #obs <- readxl::read_excel('../data/Data_Input/Complete Tag History SY2015 Chinook 9-6-17.xlsx')

## ------------------------------------------------------------------------
#  #------------------------------------------------------------------------------
#  # load configuration files
#  #------------------------------------------------------------------------------
#  config <- readxl::read_excel('../data/Config_Files/Data_Site_Config_RO_9-6-17.xlsx')
#  parentchild <- readxl::read_excel('../data/Config_Files/Parent-Child Table RO 9-6-17.xlsx')

## ---- message=FALSE, warning=FALSE---------------------------------------
#  valid_obs_dat <- nodeAssign(valid_tags = valid_tags, observation = obs,
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
#  write.csv(fish_obs, file = '../data/Data_Output/fishPaths_fnc_output.csv')
#  write.csv(fish_obs2, file = '../data/Data_Output/spawnerPaths_fnc_output.csv')

