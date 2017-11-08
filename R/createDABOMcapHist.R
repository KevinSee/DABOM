#' @title Prep DABOM capture histories
#'
#' @description Take the processed capture histories (in long format) and the groupings of node, and develop a capture history in wide format. Optionally, split that capture history into smaller dataframes by grouping.
#'
#' @author Kevin See
#'
#' @param proc_ch capture history as returned by one of the \code{processCapHist} family of functions in \code{PITcleanr} package, which has then been verified by a user and all blank UserProcStatus entries have been completed.
#' @param node_order output of function \code{createNodeOrder}
#' @param root_site Code of the node where fish were initially tagged.
#' @param split_matrices Should the wide capture history be split into separate matrices, one for each group in \code{node_order}? Default value is \code{FALSE}.
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples createDABOMcapHist()

createDABOMcapHist = function(proc_ch = NULL,
                              node_order = NULL,
                              root_site = 'GRA',
                              split_matrices = F) {

  stopifnot(!is.null(proc_ch) |
              !is.null(node_order))

  obsNodes = proc_ch %>%
    select(TagID, Node) %>%
    distinct() %>%
    filter(Node != root_site) %>%
    mutate(seen = 1) %>%
    spread(Node, seen,
           fill = 0)

  # include nodes that had no observations, to match the indexing in the DABOM JAGS model
  dabom_df = data.frame(obsNodes,
                        node_order %>%
                          select(Node) %>%
                          filter(Node != root_site,
                                 !Node %in% names(obsNodes)) %>%
                          mutate(seen = 0) %>%
                          distinct() %>%
                          spread(Node, seen)) %>%
    tbl_df()

  # # change the names
  # if(sum(grepl('^X[[:digit:]]', names(dabom_df))) > 0) {
  #   names(dabom_df)[grep('^X[[:digit:]]', names(dabom_df))] = str_replace(names(dabom_df)[grep('^X[[:digit:]]', names(dabom_df))],
  #                                                                         '^X',
  #                                                                         '')
  # }

  # add the trap date
  dabom_df = dabom_df %>%
    full_join(proc_ch %>%
                select(TagID, TrapDate) %>%
                distinct()) %>%
    select(TagID, TrapDate, one_of(node_order$Node), everything())

  if(sum(!node_order$Node %in% names(dabom_df)) > 0) {
    dabom_df[,node_order$Node[!node_order$Node %in% names(dabom_df)]] = NA
    dabom_df = dabom_df %>%
      select(TagID, TrapDate, one_of(node_order$Node), everything())
  }

  # replace all NAs with 0s
  dabom_df = dabom_df %>%
    mutate_at(vars(-c(1:2)),
              funs(ifelse(is.na(.), 0, .)))

  if(!split_matrices) return(dabom_df)

  if(split_matrices) {
    node_list = createNodeList(node_order)

    dabom_list = node_list %>%
      map(.f = function(x) {
        y = dabom_df %>%
          select(one_of(x))
      })

    return(dabom_list)
  }
}
