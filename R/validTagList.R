#' @title lgr2SCOBI: Format adult data from LGTrappingDB for SCOBI
#'
#' @description lgr2SCOBI() is a function to convert raw data exported or 'dumped' out of the LGTrappingDB to a format that is ready
#' to be analyzed by the \link{SCOBI}() function. It is to be used on one species/spawn year combination (e.g., SY2015 steelhead) at a time. It will
#' write a csv file ready to be used as the \code{adultData} argument of the SCOBI() function.
#'
#' @param input the name of the csv file containing adult data exported from the LGTrappingDB. Can also accept an \code{object} of a similar
#' format. See \code{\link[SCOBI]{exRawSthdAdultData}}
#' @param species what is the species of the data you are attempting to format for SCOBI. The default option is \code{"chnk"} for
#' Chinook salmon. Simply set as \code{"sthd"} for steelhead.
#' @param exportFile What would you like to name your exported file containing your data formatted for SCOBI analysis? By
#' default \code{lgr2SCOBI()} exports the formatted data as a csv file which is the preferred format for SCOBI.
#'
#' @author Mike Ackerman
#'
#' @examples lgr2SCOBI(input = exRawSthdAdultData, species = "sthd", exportFile = "SthdScobiInput")
#'
#' @import stringr car
#' @export
#' @return NULL

lgr2SCOBI <- function(input = NULL, species = "chnk", exportFile = NULL)
{
  # IMPORT UNFORMATTED DATA DUMPED FROM THE LGRTrappingDB
  if(is.character(input) == TRUE) { rawData <- read.table(file = input, header = TRUE, sep = ",", na.strings = c("","NA"), comment.char = "")
  } else { rawData <- input }
  data   <- subset(rawData, select = c("WeekNumber","CollectionDate","SpawnYear","MasterID","BioSamplesID","SRR","LGDMarkAD","LGDFLmm","GenSex",
                                       "GenStock","GenStockProb","BioScaleFinalAge","GenPBT_ByHat","GenPBT_RGroup","GenParentHatchery","GenBY","BiosamplesValid","LGDValid",
                                       "LGDNumPIT"))
