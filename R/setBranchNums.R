#' @title Set Branch Numbers
#'
#' @description Returns list with the number of branches at each branching node based on parent child relationship table.
#'
#' @author Kevin See
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr PITcleanr
#' @importFrom rlang set_names
#' @export
#' @return NULL
#' @examples setBranchNums()

setBranchNums = function(parent_child = NULL) {

  root_node = parent_child %>%
    PITcleanr::buildNodeOrder() %>%
    filter(node_order == 1) %>%
    pull(node)

  branchNodes = parent_child %>%
    dplyr::count(parent,
                 name = "n_child") %>%
    filter(n_child > 1) %>%
    mutate(across(parent,
                  str_replace,
                  'A0$', '')) %>%
    # add a "not there" to every branch except root
    mutate(n_child = if_else(parent == root_node,
                             n_child,
                             as.integer(n_child + 1)))

  branchNums_list = branchNodes %>%
    pull(n_child) %>%
    as.list() %>%
    rlang::set_names(paste0('n_pops_', branchNodes$parent))

  return(branchNums_list)
}
