#' @title Compile information about each node
#'
#' @description Based on a parent-child table and a configuration file, this function compiles how many nodes each site has, what the parent site is, and what child number of that parent the site in question is
#'
#' @author Kevin See
#'
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr stringr PITcleanr
#' @export
#' @return NULL
#' @examples writeDABOM()

getNodeInfo = function(parent_child = NULL,
                       configuration = NULL) {

  # determine starting point (root_site)
  root_site = PITcleanr::buildNodeOrder(parent_child) %>%
    filter(node_order == 1) %>%
    pull(node)

  # add nodes to parent-child table
  pc_nodes = addParentChildNodes(parent_child,
                                 configuration = configuration)


  # how many child sites does each parent site have?
  parent_info = parent_child %>%
    group_by(parent, parent_rkm) %>%
    mutate(n_child = n_distinct(child))

  # get the column names of the capture history matrix
  col_nms = defineDabomColNms(root_site = root_site,
                              parent_child = parent_child,
                              configuration = configuration) %>%
    unlist() %>%
    as.vector()


  # how many nodes does each site have, what are their names and what column are they contained in?
  node_info = configuration %>%
    # filter(node %in% unique(c(pc_nodes$parent, pc_nodes$child))) %>%
    filter(node %in% pc_nodes$child) %>%
    mutate(node_site = if_else(nchar(node) >= 5 & (grepl("U$", node) | grepl("D$", node)),
                               stringr::str_remove(stringr::str_remove(node, "U$"), "D$"),
                               node)) %>%
    group_by(site_code = node_site) %>%
    summarise(n_nodes = n_distinct(node),
              node = list(unique(node)),
              .groups = "drop") %>%
    unnest(cols = node) %>%
    rowwise() %>%
    mutate(matrix_col = if_else(node %in% col_nms,
                                stringr::str_which(col_nms, paste0("^", node)),
                                NA_integer_)) %>%
    ungroup() %>%
    arrange(matrix_col) %>%
    left_join(parent_child %>%
                select(site_code = child,
                       parent_site = parent),
              by = "site_code") %>%
    left_join(parent_child %>%
                split(list(.$parent)) %>%
                map_df(.id = "parent_site",
                       .f = function(x) {
                         x %>%
                           arrange(child_rkm) %>%
                           mutate(child_num = 1:n()) %>%
                           select(site_code = child,
                                  child_num)
                       }),
              by = c("site_code", "parent_site"))

  return(node_info)

}
