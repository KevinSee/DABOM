#' @title spawnerPaths: creates a data frame of all observed paths
#'
#' @description The function builds a data frame of all the observed fish paths in the
#' observation file based on the parent child table. It works by checking node observations
#' versus the previous node observation to see if the previous node was a parent or grandparent
#' great grand-parent.....
#'
#' @param valid_obs
#'
#' @param valid_paths
#'
#' @param tagCntLimit
#'
#' @author Ryan N. Kinzer
#'
#' @examples spawnerPaths()
#'
#' @import dplyr
#' @import lubridate
#' @export
#' @return NULL
#'
spawnerPaths <- function(valid_obs, valid_paths){

  node_order <- valid_paths %>%
    group_by(EndNode) %>%
    arrange(EndNode, desc(NodeNum)) %>%
    mutate(NodeOrder = 1:n()) %>%
   ungroup() %>%
   distinct(NodeSite, .keep_all=TRUE) %>%
    select(NodeSite, NodeOrder)  %>%
    full_join(valid_paths %>%
                filter(!is.na(pathString)) %>%
                select(NodeSite, pathString), by = 'NodeSite')


      allObs <- valid_obs %>%
      left_join(node_order, by = c("Node" = "NodeSite")) %>%
      group_by(TagID) %>%
      mutate(previous_node = lag(Node),
             next_node = lead(Node),
             prev_string = lag(pathString),
             next_node = ifelse(is.na(next_node),Node, next_node)) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(Up = ifelse(is.na(previous_node), 'Up',
                                    ifelse(grepl(previous_node, pathString),'Up', NA)),
             Down = ifelse(grepl(Node,prev_string),'Down', NA ),
             Hold = ifelse(Node == previous_node, 'Hold', NA),
             Direction = ifelse(!is.na(Up), Up, Down),
             Direction = ifelse(!is.na(Hold), 'Hold', Direction)) %>%
      select(names(valid_obs), NodeOrder, Direction) %>%
      ungroup()

#
#     allObs_2 <- valid_obs %>%
#       left_join(node_order, by = c('Node' = 'NodeSite')) %>%
#       group_by(TagID) %>%
#       mutate(previous_order = lag(NodeOrder)) %>%
#       ungroup() %>%
#       rowwise() %>%
#       mutate(Direction = ifelse(Node == 'GRA', 'Up',
#                                 ifelse(NodeOrder > previous_order, 'Up',
#                                        ifelse(NodeOrder < previous_order, 'Down', NA)))) %>%
#       select(TagID, ObsDate, Node, NodeOrder, Direction)


    df <- allObs %>%
    filter(is.na(Direction)) %>%
    distinct(TagID) %>%
    mutate(ValidPath = rep(FALSE,n()))

  allObs <- left_join(allObs, df) %>%
    mutate(ValidPath = ifelse(is.na(ValidPath),TRUE,FALSE))


  modObs <- allObs %>%
    filter(Direction == 'Up') %>%
    group_by(TagID) %>%
    distinct(Node, .keep_all=TRUE) %>%
  slice(1:which.max(NodeOrder)) %>%
    mutate(ModelObs = TRUE) %>%
    select(TagID, ObsDate, ModelObs)

  allObs <- left_join(allObs, modObs)
#
#   write.csv(allObs, './Data/rk_fnc_output.csv')
#


return(allObs)
} # ends function
