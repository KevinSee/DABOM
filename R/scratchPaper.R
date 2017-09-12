library(devtools)
library(dplyr)
library(lubridate)
library(readxl)

devtools::install_github("mackerman44/DABOM")

library(DABOM)

#------------------------------------------------------------------------------
# load data and configuration files
#------------------------------------------------------------------------------
validTagList(input = LGTrappingDB_03192017, spawnYear = 'SY2016', species = "chnk",
             validTagFile = "validTagFile.txt", validTagData = "validTagData.csv" )

View(LGTrappingDB_03192017)

#------------------------------------------------------------------------------
# load data and configuration files
#------------------------------------------------------------------------------
obs <- read_excel('./Data/PITCleaner input files/Complete Tag History SY2015 Chinook 9-6-17.xlsx')
config <- read_excel('./Data/PITCleaner input files/Data_Site_Config_RO_9-6-17.xlsx')
validtag <- read_excel('./Data/PITCleaner input files/2015 Chinook tag list ro 9-6-17.xlsx')
parentchild <- read_excel('./Data/PITCleaner input files/Parent-Child Table RO 9-6-17.xlsx')
#------------------------------------------------------------------------------
# assign node names to all records in observation file, returns all records in
# obs file.  Object will need to be trimmed based on ValidDate and ValidNode
# before mapping the acceptable paths, because non-valid nodes or nodes
# observed prior to valid dates may not be in the parent-child table
#------------------------------------------------------------------------------
#full_obs_dat <- nodeAssign(valid_tags = validtag, observation = obs,
#                           configuration = config, truncate = FALSE)

valid_obs_dat <- nodeAssign(valid_tags = validtag, observation = obs,
                           configuration = config, truncate = TRUE)

#write.csv(full_obs_dat, file = './Data/full_obs_dat.csv')
#write.csv(valid_obs_dat, file = './Data/valid_obs_dat.csv')

#------------------------------------------------------------------------------
# create a data frame with all valid paths identified in the parent child table
valid_paths <- validPaths(parent_child = parentchild)
#------------------------------------------------------------------------------
# map all valid observations to valid paths - uses Greg's process function
# and label as autoprocess, etc....
fish_obs <- fishPaths(valid_obs_dat, valid_paths)

fish_obs %>%
  filter(UserProcStatus == '') %>%
  distinct(TagID) %>%
  summarise(n())

# figure out if fish movement is upstream or downstream, and in the same path
# then truncate to only model observations
# RK and Rick thought process.
fish_obs2 <- spawnerPaths(valid_obs, valid_paths)

fish_obs2 %>%
    filter(is.na(Direction)) %>%
    distinct(TagID) %>%
    nrow()

#write.csv(fish_obs, './Data/fish_obs.csv')

