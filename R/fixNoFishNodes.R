#' @title Modify basic JAGS model
#'
#' @description Takes the basic JAGS model and modifies it based on particular observations. Specifically, it fixes the detection probability of all nodes that had no observations to 0.
#'
#' @author Kevin See
#'
#' @param init_file name (with file path) of basic JAGS model.
#' @param file_name name (with file path) to save the model as.
#' @param proc_ch capture history as returned by one of the \code{processCapHist} family of functions in \code{PITcleanr} package, which has then been verified by a user and all blank UserProcStatus entries have been completed.
#' @param node_order output of function \code{createNodeOrder}
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples fixNoFishNodes()

fixNoFishNodes = function(init_file = NULL,
                          file_name = NULL,
                          proc_ch = NULL,
                          node_order = NULL) {

  stopifnot(!is.null(init_file) |
              !is.null(file_name) |
              !is.null(proc_ch) |
              !is.null(node_order))

  # which nodes had observations?
  seenNodes = proc_ch %>%
    filter(UserProcStatus) %>%
    select(Node) %>%
    distinct() %>%
    as.matrix %>%
    as.character()

  # which nodes did not?
  unseenNodes = unique(node_order$Node)[!unique(node_order$Node) %in% seenNodes]

  # convert to site codes
  seenSites = str_replace(seenNodes, 'B0$', '') %>%
    str_replace('A0$', '') %>%
    unique()

  unseenSites = str_replace(unseenNodes, 'B0$', '') %>%
    str_replace('A0$', '') %>%
    unique()


  # read in basic model file
  # open a connection
  mod_conn_init = file(init_file, open = 'r+')
  mod_file_org = readLines(mod_conn_init)
  close(mod_conn_init)
  rm(mod_conn_init)

  mod_file = mod_file_org
  # open a connection to new model file
  mod_conn_new = file(file_name, open = 'w')

  for(node in unseenNodes) {
    mod_file[grep(paste0(node, '_p'), mod_file)[1]] = paste0('  ', node, '_p <- 0 # no detections / not in operation')
  }

  if(length(unseenNodes) > 0) {
    cat(paste('Fixed', paste(unseenNodes, collapse = ', '), 'at 0% detection probability, because no tags were observed there.\n'))
  }

  # for(site in unseenSites) {
  #   if(sum(grepl(paste0('phi_', tolower(site)), mode_file)) > 0)
  #     mode_file[grep(paste0('phi_', tolower(site), ' ~'))] = paste0('phi_', tolower(site), '_p <- 0 # no detections here or upstream')
  # }

  # check if some sites only saw fish at single array (i.e. double array not installed that year)
  # if that site is the furtherest upstream, fix the detection probability at 100%
  seenSites = str_replace(seenNodes, 'B0$', '') %>%
    str_replace('A0$', '') %>%
    unique()

  unseenSites = str_replace(unseenNodes, 'B0$', '') %>%
    str_replace('A0$', '') %>%
    unique()

  singleSites = intersect(seenSites, unseenSites)

  for(site in singleSites) {
    tmp = node_order %>%
      filter(grepl(site, Path),
             Node %in% seenNodes) %>%
      group_by(Node) %>%
      summarise(maxNodeOrder = max(NodeOrder))


    if(tmp %>%
       filter(maxNodeOrder == max(maxNodeOrder),
              grepl(site, Node)) %>%
       nrow() > 0) {

      mod_file[grep(paste0(seenNodes[grepl(site, seenNodes)], '_p'), mod_file)[1]] = paste0('  ', seenNodes[grepl(site, seenNodes)], '_p <- 1 # Single array, no upstream detections')

      cat(paste('\nFixed', site, 'at 100% detection probability, because it is a single array with no upstream detections.\n'))

    }

  }

  writeLines(mod_file, mod_conn_new)
  close(mod_conn_new)

}
