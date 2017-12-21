#' @title nodeAssign: Assigns PIT-tag observation node site names to PTAGIS tag detection history
#' records.
#'
#' @description The function assigns PIT-tag observation node site names to each
#' tag detection history record generated from a PTAGIS 'complete tag history query'.  The
#' complete tag history query is completed by running the tag list outputted from the
#' validTagList() function. Observation node site names are assigned from joining a configuration
#' file with the PTAGIS query results on 'Site_Code' and 'AntennaID' fields. The configuration
#' file, 'siteDescription.csv', is distributed and maintained within the DABOM package.
#'
#' An observation node is a single or group of PIT-tag antenna that, at the finest resolution,
#' act as a unique tag detection location.  Often, a node consists of multiple antennas at a unique
#' PTAGIS interogation site, which together form a single array.  In addition, multiple nodes
#' may exist at one PTAGIS interogation site or mark-release-recapture (MRR) site when more than
#' antenna array are assigned to the same SiteID. A node may also be a single antenna or coil
#' located in an adult ladder or trap entry or the single (MRR) SiteID for fish handled and
#' scanned at a weir location.
#'
#' @param valid_tags is a data frame or comma seperated value (.csv) file containing the
#' valid tag list which is outputted from the validTagList() function and the LGTrappingDB
#'
#' @param observation is the PTAGIS observation file inputted as a data frame or .csv file
#' containing the complete tag history for each of the tagIDs in valid_tags
#'
#' @param configuration is a data frame or .csv file which assigns node names to unique SiteID,
#' AntennaID, and site configuration ID combinations.
#'
#' @param truncate logical, subsets observations to those with valid nodes, observations dates
#' greater than trapping date at LGD and then to the minimum observation date of each set of
#' observation events at a node, multiple observation events can occur at one node if the
#' observations are split by detections at other nodes
#'
#' @author Ryan Kinzer
#'
#' @examples nodeAssign()
#'
#' @import dplyr
#' @import readr
#' @export
#' @return NULL

nodeAssign <- function(valid_tags, observation, configuration, truncate = FALSE){
  # IMPORT .csv files or load data from an R data frame object

  if(is.character(valid_tags) == TRUE) {
    validtag <- read_csv(file = valid_tags, header = TRUE, sep =',')
  }
  else {
    validtag <- valid_tags
  }

  if(is.character(observation) == TRUE) {
    obs <- read_csv(file = observation, header = TRUE, sep =',')
  }
  else {
    obs <- observation
  }

  if(is.character(configuration) == TRUE) {
    config <- read_csv(file = configuration, header = TRUE, sep =',')
  }
  else {
    config <- configuration
  }

  obs_df <- obs %>%
    mutate(ObsDate = ifelse(is.na(`Event Release Date Time Value`),`Event Date Time Value`,
                                       `Event Release Date Time Value`),
           `Event Date Time Value` = as.POSIXct(`Event Date Time Value`, format = '%m/%d/%Y %H:%M'),
           `Event Release Date Time Value` = as.POSIXct(`Event Release Date Time Value`, format = '%m/%d/%Y %H:%M'),
          ObsDate = as.POSIXct(ObsDate, format = '%m/%d/%Y %H:%M')) %>%
    select(TagID = `Tag Code`,
           ObsDate,
           SiteID = `Event Site Code Value`,
           AntennaID = `Antenna ID`,
           ConfigID = `Antenna Group Configuration Value`,
           everything()) %>%
    left_join(select(validtag, TagID, TrapDate),
              by = c('TagID')) %>%
    mutate(ValidDate = ifelse(ObsDate > TrapDate, TRUE, FALSE)) #%>%
    #filter(ValidDate == TRUE) %>%
    #select(-ValidDate)

  tmp_df <- obs_df %>%
    distinct(SiteID, AntennaID, ConfigID) %>%
    anti_join(config %>%
                distinct(SiteID, AntennaID, ConfigID))

  if( nrow(tmp_df) > 0 ){

    cat( "The following SiteID - AntennaID - ConfigID combinations are in the observation file
         but not listed in the site configuration file.\n")

    for( i in 1: nrow(tmp_df) ){

      print( paste0(tmp_df$SiteID[i], " - ", tmp_df$AntennaID[i], " - ", tmp_df$ConfigID[i]))
    }

    cat("Observation records with these combinations are flagged with an 'ERROR' in the Node field")
  }

obs_dat <- obs_df %>%
  left_join(select(config,
                   SiteID,
                   AntennaID,
                   ConfigID,
                   Node,
                   ValidNode,
                   AntennaGroup,
                   ModelMainBranch,
                   SiteName,
                   SiteDescription),
                   by = c('SiteID', 'AntennaID', 'ConfigID')) %>%
  mutate(Node = ifelse(is.na(Node), 'ERROR', Node),
         ValidNode = ifelse(is.na(Node), FALSE, ValidNode)) %>%
  arrange(TagID, ObsDate)


# if(truncate == TRUE){
#
  # valid_obs <- obs_dat %>%
  #   filter(ValidDate == TRUE,
  #          ValidNode == TRUE) %>%
  #   mutate(min_obs = NA)
#
# #   # test
# # tmp <- valid_obs %>%
# #   group_by(TagID, Node) %>%
# #   slice(which.min(ObsDate))
# #   # end test
#
#   iloop <- nrow(valid_obs)
#
#   for(i in 2:iloop){
#    valid_obs$min_obs[1] <- TRUE
#     if(valid_obs$TagID[i] != valid_obs$TagID[i-1]){ valid_obs$min_obs[i] <- TRUE
#     } else {
#       if(valid_obs$Node[i] != valid_obs$Node[i-1]) { valid_obs$min_obs[i] <- TRUE
#        } else {valid_obs$min_obs[i] == FALSE}
#     }
#   } # iloop
#
#   obs_dat <- valid_obs %>%
#     filter(min_obs == TRUE) %>%
#     select(-min_obs)
#
# } # truncate if statement

# another way, without the loop
if(truncate == TRUE){

  obs_dat = obs_dat %>%
    filter(ValidDate == TRUE,
           ValidNode == TRUE) %>%
    group_by(TagID) %>%
    mutate(prev_node = lag(Node)) %>%
    filter(Node != prev_node | is.na(prev_node)) %>%
    select(-prev_node) %>%
    ungroup()

} # truncate if statement

return(obs_dat)
}
