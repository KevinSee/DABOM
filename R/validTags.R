#' @title validTags: select valid tags from Lower Granite trapping data
#'
#' @description validTags() is a function to subset only valid tags
#' from the LGTrappingExportJodyW.accdb database. A dataframe,
#' outputted from the LGTrapData() function, is supplied and then trimmed
#' for only those records deemed valid (by IDFG), being AD-Intact and having
#' a PIT-tag number.  Chinook are also trimmed to only those fish with a run
#' designation of '5' in the SRR field.
#'
#' @param trapdata data frame outputted from LGTrapData() function
#'
#' @author Ryan N. Kinzer
#'
#' @examples validTags(trapdata = trap_df)
#'
#' @import dplyr
#' @export
#' @return NULL
validTags <- function(trapdata){

  spp <- unique(trapdata$LGDSpecies)

  if(spp == '1'){
    df <- trapdata %>%
      filter(LGDValid == 1,
             LGDMarkAD == 'AI',
             !is.na(LGDNumPIT),
             grepl('5',SRR))
  }

  if(spp == '3'){
    df <- trapdata %>%
      filter(LGDValid == 1,
             LGDMarkAD == 'AI',
             !is.na(LGDNumPIT))
  }

  df <- df %>%
    rename(TagID = LGDNumPIT, TrapDate = CollectionDate)

return(df)
}
