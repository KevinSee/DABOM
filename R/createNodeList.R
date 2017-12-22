#' @title Node List
#'
#' @description Takes the node order dataframe and converts it to a list with an entry for each "group" in the node order dataframe. Each entry in the list contains a character vector of the nodes within that group.
#'
#' @author Kevin See
#'
#' @param node_order output of function \code{createNodeOrder}
#'
#' @import dplyr purrr
#' @export
#' @return NULL
#' @examples createNodeList()

createNodeList = function(node_order = NULL) {

  stopifnot(!is.null(node_order))

  node_order %>%
    split(list(.$Group)) %>%
    map(.f = function(x) {
      x %>%
        arrange(RKM) %>%
        select(Node) %>%
        distinct() %>%
        as.matrix() %>%
        as.character()
    })
}
