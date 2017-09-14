#' @title validTagList: Generate valid PIT tag list from LGTrappingDB
#'
#' @description validTagList() is a function to generate a valid tag list using the LGTrappingDB.
#'
#' @param input the name of the tab-delimited text file containing all of the records from the LGTrappingDB.
#' @param spawnYear Which spawn year do you want to pull records for? Should be in character format. For example, to
#' pull records for spawn year 2016 use \code{"SY2016"}.
#' @param species which species?
#' @param validTagFile name of valid tag list file? Will be exported as tab delimited .txt
#' @param validTagData name of file for all data associated with the valid tag list
#'
#' @author Mike Ackerman
#'
#' @examples validTagList(input = LGTrappingDB_03192017, spawnYear = "SY2016", species = "chnk", exportFile = "sy2016chnkValidTagList.txt") )
#'
#' @import dplyr
#' @export
#' @return NULL

validTagList <- function(input = NULL, spawnYear = 'SY2016', species = 'chnk',
                         validTagFile = 'validTagFile.txt', validTagData = 'validTagData.csv')
{
  if(spp == '1'){
    df <- df %>%
      filter(LGDValid == 1,
             LGDMarkAD == 'AI',
             !is.na(LGDNumPIT),
             grepl('5',SRR))
  }

  if(spp == '3'){
    df <- df %>%
      filter(LGDValid == 1,
             LGDMarkAD == 'AI',
             !is.na(LGDNumPIT))
  }

  # 10. GRAB THE VALID TAG LIST
  validTagList <- as.character(LGTrapDB$LGDNumPIT)
  write.table(validTagList, file = validTagFile, quote = FALSE, sep = '\t',
              row.names = FALSE, col.names = FALSE)

  # 11. EXPORT ALL OF THE DATA FOR THE VALID TAG LIST
  write.csv(LGTrapDB, file = validTagData, row.names = FALSE)
}



