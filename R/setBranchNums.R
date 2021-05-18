#' @title Set Branch Numbers
#'
#' @description Returns list with the number of branches at each branching node based on parent child relationship table.
#'
#' @author Kevin See
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples setBranchNums()

setBranchNums = function(parent_child = NULL) {

  parent_child %>%
    dplyr::count(parent,
                 name = "n_child") %>%
    filter(n_child > 1) %>%
    tidyr::pivot_wider(names_from = "parent",
                       values_from = "n_child") %>%
    as.list() %>%
    return()

}
