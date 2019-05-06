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

  # dataframe of sites and nodes, and which nodes had at least one detection
  node_detects = node_order %>%
    select(NodeSite, Node) %>%
    full_join(proc_ch %>%
                filter(UserProcStatus) %>%
                select(Node) %>%
                distinct() %>%
                mutate(seen = T),
              by = 'Node') %>%
    mutate_at(vars(seen),
              funs(if_else(is.na(.), F, .)))

  # which nodes had observations?
  seenNodes = node_detects %>%
    filter(seen) %>%
    select(Node) %>%
    as.matrix %>%
    as.character

  # which nodes had no observations?
  unseenNodes = node_detects %>%
    filter(!seen) %>%
    select(Node) %>%
    as.matrix %>%
    as.character

  # convert to site codes
  unseenSites = node_detects %>%
    group_by(NodeSite) %>%
    summarise(nNodes = n_distinct(Node),
              nSeen = sum(seen)) %>%
    filter(nSeen == 0) %>%
    select(NodeSite) %>%
    as.matrix %>%
    as.character

  seenSites = node_detects %>%
    group_by(NodeSite) %>%
    summarise(nNodes = n_distinct(Node),
              nSeen = sum(seen)) %>%
    filter(nSeen > 0) %>%
    select(NodeSite) %>%
    as.matrix %>%
    as.character

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

  # which sites have multiple nodes but only one had detections?
  singleSites = node_detects %>%
    group_by(NodeSite) %>%
    summarise(nNodes = n_distinct(Node),
              nSeen = sum(seen)) %>%
    filter(nSeen == 1,
           nNodes > nSeen) %>%
    select(NodeSite) %>%
    as.matrix %>%
    as.character

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

  # if no observations at some terminal nodes, fix the movement probability past those nodes to 0
  if(sum(grepl('phi', mod_file)) > 0) {
    phiNodes = tibble(modLines = str_trim(mod_file[grep('phi_', mod_file)])) %>%
      mutate(site = str_split(modLines, '\\~', simplify = T)[,1]) %>%
      select(site) %>%
      mutate(site = str_trim(site)) %>%
      filter(grepl('phi', site)) %>%
      distinct() %>%
      mutate(site = str_replace(site, '^phi_', '')) %>%
      as.matrix() %>%
      as.character()

    if(sum(grepl('\\[', phiNodes)) > 0) {
      phiSites = str_split(phiNodes, '\\[', simplify = T)[,1]
    } else {
      phiSites = phiNodes
    }

    unseenPhiSites = intersect(str_to_upper(phiSites), unseenSites)
    unseenNodePaths = unseenPhiSites %>%
      as.list() %>%
      map_df(.f = function(x) {
        node_order %>%
          filter(grepl(x, Path))
      })
    if(sum(!unseenNodePaths$NodeSite %in% unseenPhiSites) > 0) {
      pathDf = unseenNodePaths %>%
        filter(!NodeSite %in% unseenPhiSites) %>%
        select(NodeSite) %>%
        distinct() %>%
        as.matrix() %>%
        as.character() %>%
        as.list() %>%
        map_df(.f = function(x) {
          node_order %>%
            filter(grepl(x, Path))
        })

      for(site in unseenPhiSites) {
        if(sum(grepl(site, pathDf$Path)) > 0 ) {
          unseenPhiSites = unseenPhiSites[-match(site, unseenPhiSites)]
        }
      }
    }

    phi_df = tibble(node = phiNodes,
                    site = toupper(phiSites)) %>%
      inner_join(tibble(site = intersect(unseenPhiSites, unseenSites)))

    if(nrow(phi_df) > 0 ) {
      for(i in 1:nrow(phi_df)) {
        # phi_prior = paste0('phi_', phi_df$node[i], ' ~')
        mod_file[grep(paste0('phi_', tolower(phi_df$site[i])), mod_file)[1]] = paste0('  phi_', phi_df$node[i], ' <- 0 # no upstream detections')

        cat(paste('\nFixed upstream movement past site', phi_df$site[i], 'to 0 because no detections there or upstream.\n'))

      }
    }
  }

  if('STR' %in% unseenSites & 'KRS' %in% seenSites) {
    mod_file[grep('KRS_p ~', mod_file)] = 'KRS_p <- 1 # Single array, no upstream detections'
  }

  if('LRL' %in% unseenSites & 'FISTRP' %in% seenSites) {
    mod_file[grep('phi_fistrp ~', mod_file)] = 'phi_fistrp <- 1 # no detections at LRL'
  }

  if(sum(c('MTR', 'UTR', 'TUCH') %in% unseenSites) == 3 & 'LTR' %in% seenSites) {
    mod_file[grep('LTR_p ~', mod_file)] = 'LTR_p <- 1 # Single array, no upstream detections'
  }

  if('SC2A0' %in% unseenNodes & ('SC1' %in% seenNodes & 'SC2B0' %in% seenNodes)) {
    mod_file[grep('SC2B0 ~', mod_file)] = 'SC2B0 ~ dbeta(1, 1)'
  }

  writeLines(mod_file, mod_conn_new)
  close(mod_conn_new)

}
