#' @title fishPaths: creates a data frame of all observed paths
#'
#' @description The function builds a data frame of all the observed fish paths in the
#' observation file based on the parent child table. The function was originally called
#' FishPath_truncateToLastLeg() and was built by Greg K. Ryan K. altered the function
#' to work solely in R and doesn't require the SQLite backend.
#'
#' @param valid_obs
#'
#' @param valid_paths
#'
#' @param tagCntLimit
#'
#' @author Greg Kliewer
#'
#' @examples fishPaths()
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @export
#' @return NULL
#'
fishPaths <- function(valid_obs, valid_paths){
  # may need to add species
  # Species <- 'Chinook'
  # startDate <- '03/01/16'
  # endDate <- '08/16/16'
  #Requests:
  #Pre-script data check--e.g. Array in config or obs use lower case whereas in parent-child use all upper
  #DONE
  #add final userStatus to obs output: if any auto)rocess = "no" for entire tag history, set userStatus = null, else userStatus = "process"
  #Species and Date range as input parameter
  #Debug: miscalc of finalObsLoc in csv output--cf SFG-SFG
  #seeming miscalculations of main vs ext vs outside: see same tag 3D9.1C2DAC419A, which is maybe right but is good starting point

  #ct <- Sys.time()
  #print(str_c("start time: ", ct))

  #assume ending dates need time (11:59:59PM) added for all equivalency calculations to be accurate
  # if( length( grep( "/:", startDate)) == 0 ) startDate <- paste0(startDate, " 00:00:00")
  # if( length( grep( "/:", endDate)) == 0 ) endDate <- paste0(endDate, " 23:59:59")
  #
  # seasonBegin <- substr(mdy( seasonBegin ),1,10)
  # seasonEnd <- substr(mdy_hms( seasonEnd ),1,19)
  # spawnBegin <- substr(mdy( spawnBegin ),1,10)
  # spawnEnd <- substr(mdy_hms( spawnEnd ),1,19)
  #
  # dbConn <- dbConnect(RSQLite::SQLite(), "db_pitClean.sqlite")
  #
  # #load and validate higher order data
  # loadData_HigherOrder(dbConn)
  # if(  TRUE ) #RK commented out down to #load observational data
  # {
  #   dataIsValid <- validateData_HigherOrder(dbConn)
  #   if( !dataIsValid ){ stop("Invalid data in config and/or parent-child file/table.  See console for details.") }
  # }
  #
  # #load observational data
  # loadData_Observational(dbConn)
  #
  # #populate array values
  # #note: this query assigns nulls wherever there is no matching siteID, which is ok in this context
  # #to prevent nulls add: Where exists (Select Array FROM Site_Config Where SiteID = Obs.SiteID AND AntennaID = Obs.AntennaID)
  # qry <- "Update Obs Set Array = (Select Array FROM Site_Config Where SiteID = Obs.SiteID AND AntennaID = Obs.AntennaID)"
  # dbSendStatement(dbConn, qry)
  #
  # #after above, records with null arrays must be outside study area
  # qry <- "Update Obs Set OutsideStudyArea = 'x' where Array is null OR Array = 'OUTSIDE STUDY AREA'"
  # dbSendStatement(dbConn, qry)
  #
  # #populate temporary observational table for current date ranges and do pre-processing
  # if( TRUE )
  # {
  #   qry = "Delete from ObsTemp"
  #   dbSendStatement(dbConn, qry)
  #
  #   qry <- str_c("Insert into ObsTemp( SiteID, Array, AntennaID, TagID, ObsDateTime, Species )
  #                SELECT SiteID, Array, AntennaID, TagID, ObsDateTime, Species
  #                FROM Obs
  #                where Species = '", Species, "' and OutsideStudyArea is null ")
  #   qry <- str_c(qry, " And ObsDateTime >= '", seasonBegin, "' and ObsDateTime < '", seasonEnd, "'")
  #   dbSendStatement(dbConn, qry)
  #
  #   #validate observational data
  #   if(  TRUE )
  #   {
  #     dataIsValid <- validateData_Obs(dbConn)
  #     if( !dataIsValid ){ stop("Invalid data in observational data.  See console for details.") }
  #   }
  # }
  #
  # #list of all possible shortest paths-to-root (i.e. node straight to root) for current data set
  # if( TRUE ) {  shortestPathToRoot_logCurrPaths(dbConn) }
#
#   qry <- "Select StartSite, StartArray, EndSite, EndArray, NodeNum, NodeSite, NodeArray from Site_PathsFromMainRoot   "
#   pathList <- dbGetQuery(dbConn, qry)
#
#   qry <- "Select * from ObsTemp "
#   dfTagObsContainer <- dbGetQuery(dbConn, qry)

  # qry <- "Select distinct TagID from ObsTemp  "
  # #qry <- "Select distinct TagID from ObsTemp where and tagID = '3D9.1C2DB1F855'"
  # dfTag <- dbGetQuery(dbConn, qry)



  #Create tag-site matrix, with 1 row per tagID and 1 column for each study area Node at which at least 1 fish was observed
  #Note: must be dataframe, which is recursive, to use $ for referencing elements.  vectors and matrices are atomic, not recursive
  # dfCol <- dbGetQuery(dbConn, "Select distinct SiteID || '-' || Array from ObsTemp
  #                     Where SiteID || '-' || Array in
  #                     (Select ChildSite || '-' || ChildArray From Site_ParentChild)
  #                     Or
  #                     SiteID || '-' || Array in
  #                     (Select ParentSite || '-' || ParentArray From Site_ParentChild)")
  # dfTagSiteMatrix <- matrix(, nrow = nrow(dfTag), ncol = nrow(dfCol) + 5)
  # dfTagSiteMatrix <- as.data.frame(dfTagSiteMatrix)


  #
  # #Assign column names 1 - 5
  # #So either works: dfTagSiteMatrix[2, "GRA_GRA"] or dfTagSiteMatrix$GRA_GRA[2]
  # names(dfTagSiteMatrix)[1] <- "TagID"
  # names(dfTagSiteMatrix)[2] <- "ObsCnt"
  # names(dfTagSiteMatrix)[3] <- "Status"
  # names(dfTagSiteMatrix)[4] <- "FinalObsLoc"
  # names(dfTagSiteMatrix)[5] <- "FinalObsDate"
  #
  # #Assign column names 6 - n, using nodes (siteID-Array pairs) for column names
  # for(i in 1:nrow(dfCol)){
  #   names(dfTagSiteMatrix)[i+5] <- dfCol[i,1]
  # }

  alltagObs <- valid_obs %>%
    mutate(InMainPath = NA,
           InExtendedPath = NA)
           #AutoProcStatus = NA,
           #UserProcStatus = NA,
           #UserComment = NA)
           #OutsideExtPath = NA)
           #OutsideStudyArea = NA, # don't need
           #CodeComment = NA, # don't need
           #UserComment = NA,
           #MainPath = NA, # don't need
           #ExtendedPath = NA) # don't need

  tags <- distinct(alltagObs, TagID)

  # nodes <- distinct(valid_paths, NodeSite, .keep_all = TRUE) %>%
  #   full_join(distinct(alltagObs, Node), by = c('NodeSite' = 'Node')) %>%
  #   group_by(EndNode) %>%
  #   arrange(desc(NodeNum))

  # tag_mat <- as.data.frame(matrix(,nrow = nrow(tags), ncol = nrow(nodes) + 5)) #would be nice to order columns at some point
  # names(tag_mat) <- c('TagID', 'ObsCnt', 'Status', 'FinalObsNode', 'FinalObsDate', nodes$NodeSite)
  #
  # tag_mat <- tag_mat %>%
  #   mutate_at(vars(5:(nrow(nodes)+5)), funs(ymd_hms))

  totalObs = 0
  tocIndex = 0

  print(paste0(Sys.time(), "  ", 1, " of ", nrow(tags), " tags. Current tag: ",tags$TagID[1]))

  #for each tag
  for (i in 1:nrow(tags)) { # goes to line 369
    if( i %% 1000 == 0){ print(paste0(Sys.time(), "  ", i, " of ", nrow(tags), " tags. Current tag: ",tags$TagID[i]))  }

    #if( i %% tagCntLimit == 0) break

    #observations for current tag excluding those not in study area
    # qry = str_c("Select * from ObsTemp where tagID = '", dfTag$TagID[i], "' Order by ObsDateTime")
    # dfTagObs <- dbGetQuery(dbConn, qry)

    tagObs <- alltagObs %>%
      filter(TagID == tags$TagID[i]) %>% #174
      arrange(ObsDate)

    obsColCnt = ncol(tagObs)
    obsRowCnt = nrow(tagObs)
    totalObs = totalObs + obsRowCnt

    lastNode_currTag = tagObs$Node[obsRowCnt]

    #lastArray_currTag = tagObs$Array[obsRowCnt]

    #for( j in 1:obsRowCnt ) print(str_c(j, " ", dfTagObs$SiteID[j], "-", dfTagObs$Array[j] ))

    #confirm path for current tagID from list of paths.
    pathFound <- FALSE
    for( pIndex in 1:nrow(valid_paths) ){
      if( valid_paths$EndNode[pIndex] == lastNode_currTag)
      {
        pathFound <- TRUE
        break
      }
    }

    #current path not found: some kind of problem
    if (!pathFound){
      stop(str_c("Path to Root not found for Node <", lastNode_currTag, ">.  Program terminating"))
    } else {
      # qry <- str_c("Select NodeSite, NodeArray, NodeNum, PathString
      #              From Site_PathsFromMainRoot
      #              Where EndSite = '", lastSite_currTag, "' AND EndArray = '", lastArray_currTag, "' Order By NodeNum")
      # pathNodes <- dbGetQuery(dbConn, qry)
      # pathMain <- pathNodes$PathString[1]
      # pathExt <- ""
      #print(str_c(tocIndex, "  ", pathMain))

      pathNodes <- valid_paths %>%
        filter(EndNode == lastNode_currTag) %>%
        arrange(NodeNum)

      pathMain <- pathNodes$pathString[1]
      pathExt <- ""
    }

    SomeNodesNotFound = FALSE
    for( iObs in 1:obsRowCnt ){
      #print(str_c("Tag: ",dfTagObs$TagID[iTag], "   ", dfTagObs$SiteID[iTag], "-", dfTagObs$Array[iTag]))

      currTagNodeFound = FALSE
      for( iNode in 1:nrow(pathNodes) )
      {
        #print(str_c("Node: ", dfTagObs$SiteID[iTag], "-", dfTagObs$Array[iTag], "   ", pathNodes$NodeSite[iNode], "-", pathNodes$NodeArray[iNode] ))

        #use string search to determine if current observation <siteID, Array> is in current main path
        if( tagObs$Node[iObs] == pathNodes$NodeSite[iNode])
        {
          #print("currTagNodeFound")
          tagObs$InMainPath[[iObs]] = "x"
          currTagNodeFound == TRUE
          break
        }
      }
      if( !currTagNodeFound )  SomeNodesNotFound = TRUE
    } # ends iObs

    if( SomeNodesNotFound ){
      #print( str_c("NodeNotFound for:  ", dfTag$TagID[i], "   Last Main Path Node: ", lastSite_currTag, "-", lastArray_currTag ))
      # print( str_c("pathMain:  ", pathMain) )
      pLen = nchar(pathMain) + 1

      # qry <- str_c(     "Select EndSite, EndArray, trim(substr(PathString, ", pLen, ", 500)) as candExt from Site_PathsFromMainRoot " )
      # qry <- str_c(qry, "Where NodeNum = 1 And PathString like '%", pathMain, "%' Order by trim(substr(PathString, ", pLen, ", 500))")
      # #print(qry)
      # dfCandExt <- dbGetQuery(dbConn, qry)

      dfCandExt <- valid_paths %>%
        filter(NodeNum == 1,
               grepl(pathMain, pathString)) %>%
      mutate(candExt = substr(pathString,pLen,500)) %>%
      arrange(candExt)

      #From list of candidates, select the extension with latest ObsDateTime.  This will be the "preferred" extension for the purpose of tag cleaning.
      highestBidder = 0
      localNodeNum = ""
      latestObsDT = "1900-01-01 11:11:11"

      # TO DO: this non-recursive routinge won't get proper extension to extended path
      # so suppose lastObs = Root A, though fish spawned at C after following very circuitous path first (C D E) then (C F G) then (C H I)
      # this will get A as mainPath and (B C) as extended path but miss (C H I) as extension of extended path
      # so still neeed recursive work-around or recursion
      # work-around: do several more iterations below of:
      # dfCandExt <- dbGetQuery(dbConn, ""Select from from Site_PathsFromMainRoot where PathString like pathMain +
      # " " +
      # dfCandExt$EndSite[highestBidder] +
      # "-"
      # dfCandExt$EndSite[highestBidder])
      # key is that C above, eg., must be the root for any extension of the extension
      # so suppose fish went (I J) and then (I K). We'd want (C H I K) in later iteration
      # maybe better to work backwards as follows:
      # beginning with lastObs as currEndNode, for each obs
      # if thisObs not on path and thisObs is descendent to currEndNode  (check parent-child table)
      # currEndNode = thisObs
      # add node to path
      # final result should be FULL proper extension to main path
      # also, since working through obs in descending fashion, don't need to reiterate
      # So, at one and same time we are managing "descension" and time: earlier than
      #each time selecting extension if any with lastestObs

      for (j in 1:nrow( dfCandExt )){
        for (k in 1:nrow(tagObs)){

          #print( str_c( k, "  Checking ", str_c(dfTagObs$SiteID[k], "-", dfTagObs$Array[k]), " in ", dfCandExt$candExt[j] ))

          #is current SiteID-Array string in string representation of the candidate extension?
          #if( str_detect( dfCandExt$candExt[j], str_c( dfTagObs$SiteID[k],"-", dfTagObs$Array[k] )))
          if(grepl(tagObs$Node[k],dfCandExt$candExt[j])){
            #print("hello A")
            #is the current date more recent than the stored date?
            if( latestObsDT < tagObs$ObsDate[k] ){
              #print("hello B")
              latestObsDT <-  tagObs$ObsDate[k]
              highestBidder = j
              #print(c(highestBidder, latestObsDT))
            }
          }
        }
      }# ends line 258


      #print(c("highestBidder", highestBidder))
      #get full list of nodes for selected extension
      if( highestBidder > 0){
        #print(str_c( "hello ", i))
        # qry <- str_c("Select NodeSite, NodeArray, pathString from site_pathsFromMainRoot where ",
        #              "EndSite = '", dfCandExt$EndSite[highestBidder], "' AND EndArray = '", dfCandExt$EndArray[highestBidder], "' ",
        #              "Order by NodeNum")
        # #print(qry)
        # dfExtPath <- dbGetQuery(dbConn, qry)
        dfExtPath <- valid_paths %>%
          filter(EndNode == dfCandExt$EndNode[highestBidder]) %>%
          arrange(NodeNum)

        #pathExt <-  dfExtPath$PathString[1]
        pathExt <-  trimws(substr(dfExtPath$pathString[1], nchar(pathMain) + 1, 250))
        #trimws(substr(y, str_length(x)+1, 44))
        #print(str_c("highest bid path extension: ", pathExt))

        #flag observations in right segment of extended path, where right segment = proper extension of main path
        for (j in 1:nrow(tagObs)){
          #print(str_c(j, dfTagObs$SiteID[j], "-", dfTagObs$Array[j] ))

          #was fish observation already assigned to main path?
          if( is.na(tagObs$InMainPath[[j]])){        #this effectively reduces search to proper extension
            for (k in 1:nrow(dfExtPath)){
              #print(c(dfTagObs$SiteID[[j]], dfExtPath$NodeSite[[k]], dfTagObs$Array[[j]], dfExtPath$NodeArray[[k]])  )

              if(tagObs$Node[[j]] == dfExtPath$NodeSite[[k]])  tagObs$InExtendedPath[[j]] = "x"
            }
          }
        } # ends line 298
      } # ends line 281
    } # ends if line 215

    #mainObs <- FALSE
    #extObs <- FALSE
    #outsideObs <- FALSE
    #for each observation of current tag: dfTagObsContainer contains rows for all tag observations
    # tocIndex is the current row in dfTagObsContainer
    # SO current tag will populate the next obsRowCnt records in dfTagObsContainer
    # once program termnates, each record should be populated
    for( j in 1:obsRowCnt ){
      tocIndex = tocIndex + 1

      for( k in 14:15 ){
        #write value for current cell of observation matrix
        alltagObs[tocIndex, k] <-  tagObs[j, k]

        # #populate tag-site matrix
        # if( is.na( tagObs$InMainPath[j] ) && is.na( tagObs$InExtendedPath[j] ))
        # {
           #tag_mat[i, tagObs$Node[j]] <- tagObs$ObsDate[j] #"Outside"
        #   alltagObs$OutsideExtPath[tocIndex] <- "x"
           #outsideObs = TRUE
        # }
        # else if( is.na( tagObs$InExtendedPath[j] ))
        # {
        #   #tag_mat[i, tagObs$Node[j]] <- tagObs$ObsDate[j]#"Main"
        #   #mainObs = TRUE
        #
        #   #dfTagObsContainer$MainPath[tocIndex] <- pathMain
        # }
        # else
        # {
        #   #tag_mat[i, tagObs$Node[j]] <- tagObs$ObsDate[j]#"Extended"
        #   #ObservedInExtPath = TRUE
        #   #extObs = TRUE
        #
        #   #dfTagObsContainer$ExtendedPath[tocIndex] <- pathExt
        # }
      } # end k loop line 324

      # if( j == 1 ){
      #   alltagObs$MainPath[tocIndex] <- pathMain
      #   if( length( pathExt ) > 0 ){
      #     alltagObs$ExtendedPath[tocIndex] <- pathExt
      #   }
      # }
    } #end j loop line 321

    #tag_mat$FinalObsNode[i]  <- tagObs$Node[obsRowCnt]
    #tag_mat$FinalObsDate[i] <- tagObs$ObsDate[obsRowCnt]
    #tag_mat$TagID[i]        <- tagObs$TagID[1]
    #tag_mat$ObsCnt[i]       <- obsRowCnt

    #obsLoc <- "Obs:"
    #if( mainObs )    obsLoc <- paste0(obsLoc, " Main")
    #if( extObs )     obsLoc <- paste0(obsLoc, " ,Ext")
    #if( outsideObs ) obsLoc <- paste0(obsLoc, " ,Outside")
    #tag_mat$Status[i]       <- obsLoc
  } # end i loop from line 141

  proc_obs <- alltagObs %>%
    mutate(AutoProcStatus = ifelse(!is.na(InMainPath), TRUE,
                                   ifelse(!is.na(InExtendedPath), TRUE, FALSE)))

  proc_obs <- proc_obs %>%
  left_join(proc_obs %>%
              filter(AutoProcStatus == FALSE) %>%
              distinct(TagID) %>%
              mutate(UserProcStatus = FALSE)) %>%
    mutate(UserProcStatus = ifelse(is.na(UserProcStatus),TRUE, ''),
           UserComment = '') %>%
    select(AutoProcStatus, UserProcStatus, TagID, MinObsDate = ObsDate, Node, UserComment)


  #dbSendStatement(dbConn, "Drop Table ObsTemp_Cleaned")
  # dbWriteTable(dbConn, "ObsTemp_Cleaned", dfTagObsContainer, overwrite = TRUE)

  #add observatons Outside Study Area to _cleaned table
  # qry <- str_c("Insert into ObsTemp_Cleaned( SiteID, Array, AntennaID, TagID, ObsDateTime, Species, OutsideStudyArea )
  #              SELECT SiteID, Array, AntennaID, TagID, ObsDateTime, Species, OutsideStudyArea
  #              FROM Obs
  #              where Species = '", Species, "' and OutsideStudyArea = 'x' ")
  # qry <- str_c(qry, " And ObsDateTime >= '", seasonBegin, "' and ObsDateTime < '", seasonEnd, "'")
  # dbSendStatement(dbConn, qry)

  # #default process status
  # dbSendStatement(dbConn, "Update ObsTemp_Cleaned Set AutoProcStatus = 'no', UserProcStatus = 'no' ")
  #
  # #update Autoprocess where appropriate
  # dbSendStatement(dbConn, "Update ObsTemp_Cleaned Set AutoProcStatus = 'yes', UserProcStatus = 'yes' where InMainPath = 'x' or InExtendedPath = 'x' ")
  #
  # #update Userprocess where appropriate: for each tag, if 1 autoProc = no all UserProc = null
  # qry <- "Update ObsTemp_Cleaned Set UserProcStatus = null where tagID in (Select distinct tagID from ObsTemp_Cleaned where AutoProcStatus = 'no')"
  # dbSendStatement(dbConn, qry)



#
#   df <- dbGetQuery(dbConn, "Select AutoProcStatus, UserProcStatus, TagID,	ObsDateTime, SiteID, Array,	AntennaID, Species,
#                    InMainPath, InExtendedPath, OutsideExtPath, OutsideStudyArea, MainPath, ExtendedPath, CodeComment, UserComment
#                    from ObsTemp_Cleaned Order by TagID, ObsDateTime, SiteID, Array, InMainPath, InExtendedPath ")
#
#   write.csv(df, file = str_c( getwd(), "/Data/Output/Output_Tag_Obs_Scrub.csv" ), na = "")
#   print(str_c("Output_Tag_Obs_Scrub.csv written to ", getwd(), "/Data/Output/" ))
#
#   dbWriteTable(dbConn, "Output_Tag_Validate", dfTagSiteMatrix, overwrite = TRUE)
#
#   write.csv(dfTagSiteMatrix, file = str_c( getwd(), "/Data/Output/Output_Tag_Validate.csv" ), na = "")
#
#   print(str_c("Output_Tag_Validate.csv written to ", getwd(), "/Data/Output/" ))
#
#   qry <- "Select distinct tagID from ObsTemp_Cleaned where tagID not in (Select distinct tagID from ObsTemp_Cleaned where AutoProcStatus = 'no')"
#   dfx <- dbGetQuery(dbConn, qry)
#   dfx
#
#   if( TRUE )
#   {
#     print(str_c("Number of tags processed: ", i))
#     print(str_c("Number of pure tags with all yes's: ", nrow(dfx)))
#     print(str_c("Number of impure tags with one or more no: ", i - nrow(dfx)))
#     print(str_c("Number of Observations in study area: ", tocIndex))
#     print(str_c("Number of Observations outside study area: ", nrow(df) - tocIndex))
#     print(str_c("Number of Observations total: ", nrow(df)))
#     print(str_c("Start Time: ", ct))
#     print(str_c("End Time: ", Sys.time()))
#   }

  return(proc_obs)
} # ends function
