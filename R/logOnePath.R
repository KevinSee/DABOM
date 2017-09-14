#' @title logOnePath: creates a data frame of one path
#'
#' @description The function builds a data frame of one potential path from the parent child
#' table. The function was originally called shortestPathToRoot_logOnePath()
#' and was built by Greg K. Ryan K. altered the function to work solely in R
#' and to not require the SQLite backend.
#'
#' @param rootNode
#'
#' @param childNode
#'
#' @author Greg Kliewer
#'
#' @examples logOnePath()
#'
#' @import dplyr
#' @import readr
#' @export
#' @return NULL
#' #log path from given site to root (mouth of river)
logOnePath <- function(rootNode, childNode){
  #Assumes rigid treee structure vs graph, where each child has exactly 1 parent
  #For input node, calculate and log path to root determined by Site_ParentChild table in project DB

  printForDebug = FALSE
  #if( childSite == "IR5" && childArray == "IR5A0") printForDebug = TRUE

  i = 1
  paths_df <- data.frame(EndNode = childNode,
                         StartNode = rootNode,
                         NodeSite = childNode,
                         NodeNum = i)

  # qryInsert <- "Insert into Site_PathsFromMainRoot (EndSite, EndArray, StartSite, StartArray, NodeSite, NodeArray, NodeNum) Values( '"
  # qryVals <- str_c(childSite, "', '", childArray, "', '", rootSite, "', '", rootArray, "', '", childSite, "', '", childArray, "',", i, ")")
  # dbSendStatement(dbConn,  str_c(qryInsert, qryVals))
  #
  # if( printForDebug ) print(str_c(i, "   ", qryInsert, qryVals))
  #
  # #get parent of input node
  # df <- dbGetQuery(dbConn, str_c("Select parentSite, parentArray from Site_ParentChild
  #                                where childSite = '", childSite, "' and childArray = '", childArray, "'"))

  df <- parentchild %>%
    filter(ChildNode == childNode) %>%
    select(ParentNode)

  rootReached = FALSE

  while (nrow(df) > 0 && rootReached == FALSE)
  {
    i = i + 1

    tmp_df <- data.frame(EndNode = childNode,
                         StartNode = rootNode,
                         NodeSite = df$ParentNode[1],
                         NodeNum = i)

    #qryVals <- str_c(childSite, "', '", childArray, "', '", rootSite, "', '", rootArray, "', '", df[1,1], "', '", df[1,2], "',",i, ")")

    paths_df <- paths_df %>%
      bind_rows(tmp_df)

    # dbSendStatement(dbConn,  str_c(qryInsert, qryVals))
    # if( printForDebug ) print(str_c(i, "   ", qryVals))

    #get parent
    #df <- dbGetQuery(dbConn, str_c("Select parentSite, parentArray from Site_ParentChild where childSite = '", df[1,1], "' and childArray = '", df[1,2], "'"))

    df <- parentchild %>%
      filter(ChildNode == df$ParentNode[1]) %>%
      select(ParentNode)

    if(nrow(df) > 0)
    {
      #if parent is root: log then terminate
      if(df$ParentNode[1] == rootNode)
      {
        i = i + 1

        # qryVals <- str_c(childSite, "', '", childArray, "', '", rootSite, "', '", rootArray, "', '", df[1,1], "', '", df[1,2], "',", i, ")")
        #
        # dbSendStatement(dbConn,  str_c(qryInsert, qryVals))
        # if( printForDebug ) print(str_c(i, "   ", qryVals))

        tmp_df <- data.frame(EndNode = childNode,
                             StartNode = rootNode,
                             NodeSite = df$ParentNode[1],
                             NodeNum = i)

        paths_df <- paths_df %>%
          bind_rows(tmp_df)

        rootReached = TRUE
      }
    }
  } #ends while loop

  #Add path string to log

  # qry <- str_c("Select * from Site_PathsFromMainRoot where EndSite = '", childSite, "' And EndArray = '", childArray, "' order by nodeNum desc")
  # df <- dbGetQuery(dbConn, qry)

  df <- paths_df %>%
    filter(EndNode == childNode) %>%
    arrange(desc(NodeNum))

  # if( printForDebug ) print(qry)
  pathString = 'GRA'

  for (i in 2:nrow(df)){
    pathString = paste0(pathString,' ',df$NodeSite[i])
    #if( printForDebug ) print(str_c(i, "  ", df$NodeNum[i], "  ", pathString))
  }

  paths_df <- paths_df %>%
    mutate(pathString = ifelse(NodeNum == 1, pathString, NA))

  # qry <- str_c("Update Site_PathsFromMainRoot Set pathString = '", trimws(pathString), "' where EndSite = '", childSite, "' And EndArray = '", childArray, "' And nodeNum = 1")
  # #qry <- str_c("Update Site_PathsFromMainRoot Set pathString = '", pathString, "' where EndSite = '", childSite, "' And EndArray = '", childArray, "' And nodeNum = 1")
  # dbSendStatement(dbConn,  qry)

  return(paths_df)
}
