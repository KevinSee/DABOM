#' @title validPaths: creates a data frame of all available paths
#'
#' @description The function builds a data frame of all the available and acceptable paths
#' from the parent child table. These available paths are then used to check the observation
#' histories of each individual tagID.  The function was originally called
#' shortestPathToRoot_logCurrPaths() and was built by Greg K. Ryan K. altered the function
#' to work solely in R and doesn't require the SQLite backend.
#'
#' @param parent_child is a data frame or comma seperated value (.csv) file containing the
#' parent-child nodes
#'
#' @author Greg Kliewer
#'
#' @examples validPaths()
#'
#' @import dplyr
#' @import readr
#' @export
#' @return NULL

#log paths from current sites (as found in current Observational dataset) to root (mouth of river)
validPaths <- function(parent_child){
  # #empty path table
  # dbSendStatement(dbConn, "Delete from Site_PathsFromMainRoot")
  #
  # #get root relative to current observational data
  # # qry <- "SELECT distinct Site_ParentChild.ParentSite, Site_ParentChild.ParentArray
  # # FROM (Site_ParentChild INNER JOIN Site_Config ON (Site_ParentChild.ParentArray = Site_Config.Array)
  # # AND (Site_ParentChild.ParentSite = Site_Config.SiteID)) INNER JOIN Obs
  # # ON (Site_Config.AntennaID = Obs.AntennaID) AND (Site_Config.SiteID = Obs.SiteID)
  # # WHERE ParentSite || ParentArray not in  (Select ChildSite || ChildArray from Site_ParentChild )"
  #
  # qry <- "SELECT distinct ParentSite, ParentArray
  # FROM Site_ParentChild Where ParentSite || ParentArray Not in (Select ChildSite || ChildArray
  # From Site_ParentChild)"
  # df <- dbGetQuery(dbConn, qry)

  if(is.character(parent_child) == TRUE)
  { parentchild <- read_csv(file = parent_child, header = TRUE, sep =',')}
  else { validtag <- parent_child}

  df <- parentchild %>%
    distinct(ParentNode) %>%
    anti_join(select(parentchild, ChildNode), by = c('ParentNode' = 'ChildNode'))

  if(nrow(df) > 1){
    cat( "Only one root can exist in the parent child table, this must be \n
         fixed before running the function successfully. The identified roots \n
         are listed below. \n")

    for( i in 1: nrow(df) ){
      print(df$ParentNode[i])
    }
  }

  rootNode <- df$ParentNode[1]

  #Get current sites excluding root
  # qry <- "SELECT distinct ChildSite, ChildArray
  # FROM (Site_ParentChild INNER JOIN Site_Config ON (Site_ParentChild.ParentArray = Site_Config.Array)
  # AND (Site_ParentChild.ParentSite = Site_Config.SiteID)) INNER JOIN Obs
  # ON (Site_Config.AntennaID = Obs.AntennaID) AND (Site_Config.SiteID = Obs.SiteID)"
  #
  # qry <- "SELECT distinct ChildSite, ChildArray
  # FROM (Site_ParentChild INNER JOIN Site_Config ON (Site_ParentChild.ChildArray = Site_Config.Array)
  # AND (Site_ParentChild.ChildSite = Site_Config.SiteID)) INNER JOIN Obs
  # ON (Site_Config.AntennaID = Obs.AntennaID) AND (Site_Config.SiteID = Obs.SiteID)"

  # qry <- "Select distinct ChildSite, ChildArray
  # From (Site_ParentChild INNER JOIN Obs ON (Site_ParentChild.ChildSite = Obs.SiteID)
  # AND (Site_ParentChild.ChildArray = Obs.Array))"
  # df <- dbGetQuery(dbConn, qry)

  df <- parentchild %>%
    distinct(ChildNode)

# Inserted Greg's logOnePath function into the loop instead of calling a seperate function
# shortestPathToRoot_logOnePath(dbConn, df$ChildSite[i], df$ChildArray[i], rootSite, rootArray)

  path_df <- NULL
  for (i in 1:nrow(df)){
    tmp_df <- logOnePath(childNode = df$ChildNode[i], rootNode)
    path_df <- bind_rows(path_df, tmp_df)
  }

  #Insert path from root to itself
  path_df <- path_df %>%
    bind_rows(data.frame(EndNode = rootNode,
                         StartNode = rootNode,
                         NodeSite = rootNode,
                         NodeNum = 1,
                         pathString = paste0(rootNode,' ',rootNode)))

# tmp <- path_df %>%
#     left_join(select(parentchild, NodeSite = ChildNode, MainBranch, Index), by = c('NodeSite'))


  # if(TRUE)
  # {
  #   qryInsert <- "Insert into Site_PathsFromMainRoot (StartSite, StartArray, EndSite, EndArray, NodeSite, NodeArray, NodeNum, pathString) "
  #   qryVals <- str_c(" Values( '", rootSite, "', '", rootSite, "', '", rootSite, "', '", rootSite, "', '", rootSite, "', '", rootSite, "', ",
  #                    1, ", '", rootSite, "-", rootSite, " ", rootSite, "-", rootSite, "')" )
  #   dbSendStatement(dbConn,  str_c( qryInsert, qryVals ))
  # }



  return(path_df)
}
