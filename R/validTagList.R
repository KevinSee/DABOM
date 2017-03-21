#' @title validTagList: Generate valid PIT tag list from LGTrappingDB
#'
#' @description validTagList() is a function to generate a valid tag list using the LGTrappingDB.
#'
#' @param input the name of the tab-delimited text file containing all of the records from the LGTrappingDB.
#' @param spawnYear the desired spawn year
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
  { lgtrappingdb <- read.table(file = input, header = TRUE, sep ='\t') }
  else { lgtrappingdb <- input }
}

