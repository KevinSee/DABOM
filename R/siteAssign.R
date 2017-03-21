#' @title siteAssign: Assign PIT-tag observation site name to PTAGIS tag detection history
#' records.
#'
#' @description siteAssign() is a function to assign PIT-tag observation site names to each
#' tag detection history record generated from a PTAGIS 'complete tag history query'. Tag
#' observation site names are assigned from joining a configuration file with the PTAGIS
#' query results on 'Site_Code' and 'AntennaID' fields.
#'
#' @param input the name of the tab-delimited text file containing all of the records from the LGTrappingDB.
#' @param spawnYear Which spawn year do you want to pull records for? Should be in character format. For example, to
#' pull records for spawn year 2016 use \code{"SY2016"}.
#' @param species which species?
#' @param exportFile name of export file?
#'
#' @author Mike Ackerman
#'
#' @examples validTagList(input = LGTrappingDB_03192017, spawnYear = "SY2016", species = "chnk", exportFile = "sy2016chnkValidTagList.txt") )
#'
#' @import dplyr
#' @export
#' @return NULL

validTagList <- function(input = NULL, spawnYear = NULL, species = 'chnk', exportFile = NULL)
{
  # IMPORT UNFORMATTED DATA DOWNLOADED FROM LGTRAPPINGDB
  if(is.character(input) == TRUE)
  { LGtrapDB <- read.table(file = input, header = TRUE, sep ='\t') }
  else { LGtrapDB <- input }

  # GRAB ONLY THOSE COLUMNS THAT WE DESIRE
  syLGTrapDB <- filter(LGTrapDB, SpawnYear == spawnYear)


}
