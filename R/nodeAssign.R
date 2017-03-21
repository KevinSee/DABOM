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
#' as a unique tag detection location.  Often, a node consists of multiple antennas forming one
#'  PIT-tag array at a PTAGIS interogation site. A node may be a single antenna located in an
#'  adult hatchery ladder. In addition, multiple nodes can exist at one PTAGIS interogation
#'  or MRR site.
#'
#' @param PTAGIS the name of the comma seperated value (.csv) file containing the PTAGIS
#' complete tag history records for each of the valid tags outputted from the
#' validTagList() function and the LGTrappingDB
#'
#' @author Ryan Kinzer
#'
#' @examples nodeAssign()
#'
#' @import dplyr
#' @export
#' @return NULL

nodeAssign <- function(PTAGIS = NULL)
{
  # IMPORT .CSV file from PTAGIS download or load data from an R object
  if(is.character(PTAGIS) == TRUE)
  { ptagis_dat <- read.csv(file = PTAGIS, header = TRUE, sep =',')}
  else { ptagis_dat <- PTAGIS }

  # join configuration file with PTAGIS detection
  dat <- dplyr::full_join(PTAGIS,config_file, by = c("Site_Code","AntennaID"))


}
