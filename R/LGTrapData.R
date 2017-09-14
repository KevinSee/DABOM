#' @title LGTrapData: retrieve Lower Granite trapping data
#'
#' @description LGTrapData() is a function to query data from the LGTrappingExportJodyW.accdb database
#'
#' @param filename filepath of the trapping database as a character string
#'
#' @author Ryan N. Kinzer
#'
#' @examples LGTrapData(filename = './data/Data_Input/LGTrappingExportJodyW.accdb',
#' species = 'Chinook', spawnyear = '2015')
#'
#' @import RODBC
#' @import dplyr
#' @export
#' @return NULL

LGTrapData <- function(filename, species = c('Chinook', 'Steelhead'), spawnyear){

# Connect to the datebase - may need to change!
  con <- odbcConnectAccess2007(filename)

  #------------------------------------------------------------------------------
  # Extract database tables
  #------------------------------------------------------------------------------
  if(species == 'Chinook'){spp <- '1'}
  if(species == 'Steelhead'){spp <- '3'}
  year <- paste0('SY',spawnyear)

  qry <- paste0("SELECT MasterID, SRR, LGDSpecies, LGDNumPIT, CollectionDate, SpawnYear, BioSamplesID, LGDFLmm, GenRear, LGDLifeStage, GenSex, GenStock, GenStockProb, GenParentHatchery, GenBY, GenPBT_ByHat, GenPBT_RGroup, BioScaleFinalAge, PtagisEventSites, PtagisLastEventSite, PtagisLastEventDate, PtagisEventLastSpawnSite, RepeatSpawner, BiosamplesValid, LGDValid, LGDInjuryiesAll, LGDMarksAll, LGDMarkAD From tblLGDMasterCombineExportJodyW WHERE SpawnYear = '",year,"' AND LGDSpecies ='",spp,"' AND LGDLifeStage = 'RF'")

  df <- sqlQuery(con, qry)

  close(con)
return(df)
} #close the function
