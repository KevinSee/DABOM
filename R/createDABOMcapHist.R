#' @title Prep DABOM capture histories
#'
#' @description Take the processed capture histories (in long format) and the groupings of node,
#' and develop a capture history in wide format. Optionally, split that capture history into
#' smaller dataframes by grouping.
#'
#' @author Kevin See
#'
#' @param filter_ch filtered capture history as returned by the `filterDetections()`
#' function in the `PITcleanr` package, which has then been verified by a user and all
#' blank or NA `user_keep_obs` entries have been completed.
#' @param parent_child data frame with at least `parent` and `child` columns.
#' Can be created with `buildParentChild()` function in the `PITcleanr` package.
#' @param configuration is a data frame which assigns node names to unique SiteID, AntennaID, and
#' site configuration ID combinations. One example can be built with the function `buildConfig`
#' @param split_matrices Should the wide capture history be split into separate matrices,
#' one for each group in `defineDabomColNms`? Default value is \code{FALSE}.
#'
#' @import dplyr tidyr PITcleanr
#' @export
#' @return NULL
#' @examples createDABOMcapHist()

createDABOMcapHist = function(filter_ch = NULL,
                              parent_child = NULL,
                              configuration = NULL,
                              split_matrices = F) {

  stopifnot(exprs = {
    !is.null(filter_ch)
    !is.null(parent_child)
    !is.null(configuration)
  })

  col_nms <- PITcleanr::defineCapHistCols(parent_child = parent_child,
                                          configuration = configuration)

  # include nodes that had no observations, to match the indexing in the DABOM JAGS model
  dabom_df <-
    filter_ch %>%
    select(tag_code, node) %>%
    distinct() %>%
    mutate(across(node,
                  ~ factor(., levels = col_nms))) %>%
    mutate(seen = 1) %>%
    tidyr::pivot_wider(names_from = "node",
                       names_sort = T,
                       names_expand = T,
                       values_from = "seen",
                       values_fill = 0)

  # add the trap date
  dabom_df = dabom_df %>%
    full_join(filter_ch %>%
                select(tag_code, start_date) %>%
                distinct(),
              by = "tag_code") %>%
    relocate(start_date,
             .after = tag_code) %>%
    arrange(tag_code)

  # should we split up into different matrices?
  if(!split_matrices) {

    return(dabom_df)

  } else {

    pc_nodes = PITcleanr::addParentChildNodes(parent_child,
                                              configuration)

    node_order = PITcleanr::buildNodeOrder(pc_nodes)

    root_site = node_order %>%
      filter(node_order == 1) %>%
      pull(node)

    col_nms_list <- DABOM::defineDabomColNms(root_site = root_site,
                                             parent_child = parent_child,
                                             configuration = configuration,
                                             second_node = F)

    dabom_list = col_nms_list %>%
      map(.f = function(x) {
        y = dabom_df %>%
          select(one_of(x))
      })
    return(dabom_list)
  }
}
