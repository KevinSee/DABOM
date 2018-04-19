#' @title Set Branch Numbers
#'
#' @description Returns list with the number of branches at each branching node based on parent child relationship table.
#'
#' @author Kevin See
#'
#' @param parent_child dataframe with at least \code{ParentNode}, \code{ChildNode} and \code{RKM} columns
#' @export
#' @return NULL
#' @examples setBranchNums()

setBranchNums = function(parent_child = NULL) {

  rootNode = parent_child %>%
    filter(ParentNode == ChildNode) %>%
    select(ParentNode) %>%
    mutate(ParentNode = str_replace(ParentNode, 'A0$', '')) %>%
    as.matrix() %>%
    as.character()

  branchNodes = parent_child %>%
    group_by(ParentNode) %>%
    summarise(nChild = n_distinct(RKM)) %>%
    filter(nChild > 1) %>%
    mutate(ParentNode = str_replace(ParentNode, 'A0$', ''))

  branchNodes = branchNodes %>%
    filter(ParentNode == rootNode) %>%
    bind_rows(branchNodes %>%
                filter(ParentNode != rootNode))

  branchNums_list = branchNodes %>%
    select(nChild) %>%
    as.matrix() %>%
    as.integer() %>%
    as.list()
  names(branchNums_list) = paste0('n_pops_', branchNodes$ParentNode)

  return(branchNums_list)
}
