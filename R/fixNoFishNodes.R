#' @title Modify basic JAGS model
#'
#' @description Takes the basic JAGS model and modifies it based on particular observations. Specifically, it fixes the detection probability of all nodes that had no observations to 0.
#'
#' @author Kevin See
#'
#' @param init_file name (with file path) of basic JAGS model.
#' @param file_name name (with file path) to save the model as.
#' @param fish_origin tibble containing `tag_code` and origin ("W" or "H")
#' of every tag. If not supplied, every tag will be assigned "W".
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr stringr PITcleanr
#' @export
#' @return NULL
#' @examples fixNoFishNodes()

fixNoFishNodes = function(init_file = NULL,
                          file_name = NULL,
                          filter_ch = NULL,
                          parent_child = NULL,
                          configuration = NULL,
                          fish_origin = NULL) {

  stopifnot(exprs = {
    !is.null(init_file)
    !is.null(file_name)
    !is.null(filter_ch)
    !is.null(parent_child)
    !is.null(configuration)
  })

  if(is.null(fish_origin)) {
    fish_origin = filter_ch %>%
      select(tag_code) %>%
      distinct() %>%
      mutate(origin = "W")
  }

  # dataframe with information about each node, including site code, parent site, how many tags were seen
  node_info = getNodeInfo(parent_child, configuration) %>%
    full_join(filter_ch %>%
                group_by(node) %>%
                summarise(n_tags = n_distinct(tag_code),
                          .groups = "drop"),
              by = "node") %>%
    filter(!is.na(site_code)) %>%
    mutate(across(starts_with("n_tags"),
                  tidyr::replace_na,
                  0)) %>%
    mutate(tags_det = if_else(n_tags > 0, T, F)) %>%
    left_join(parent_child %>%
                PITcleanr::addParentChildNodes(configuration) %>%
                PITcleanr::buildNodeOrder(),
              by = "node")

  # which nodes are upstream of each node?
  upstrm_nodes <- NULL
  for(my_node in node_info$node) {
    tmp_info <- node_info %>%
      filter(node == my_node)

    res <- tibble(node = my_node,
                  site_code = tmp_info$site_code,
                  upstrm_node = node_info %>%
                    filter(str_detect(path, my_node),
                           site_code != tmp_info$site_code,
                           node_order > tmp_info$node_order) %>%
                    pull(node))

    if(is.null(upstrm_nodes)) {
      upstrm_nodes <- res
    } else {
      upstrm_nodes <- upstrm_nodes %>%
        bind_rows(res)
    }
    rm(tmp_info, res)
  }


  # how many tags by origin were detected at each node
  node_tags_origin = filter_ch %>%
    left_join(fish_origin,
              by = "tag_code") %>%
    group_by(node, origin) %>%
    summarise(n_tags = n_distinct(tag_code),
              .groups = "drop") %>%
    mutate(origin = factor(origin, levels = c("W", 'H'))) %>%
    complete(expand(., node = node_info$node, origin), fill = list(n_tags = 0))

  # similar info, by site code
  site_info = filter_ch %>%
    left_join(node_info %>%
                select(site_code, node),
              by = "node") %>%
    group_by(site_code) %>%
    summarise(n_tags = n_distinct(tag_code),
              .groups = "drop") %>%
    full_join(node_info %>%
                select(site_code,
                       n_nodes,
                       parent_site) %>%
                distinct(),
              by = "site_code") %>%
    mutate(across(n_tags,
                  tidyr::replace_na,
                  0)) %>%
    mutate(tags_det = if_else(n_tags > 0, T, F))


  # which nodes had observations?
  seen_nodes = node_info %>%
    filter(tags_det) %>%
    pull(node)
  unseen_nodes = node_info %>%
    filter(!tags_det) %>%
    pull(node)
  # which sites had observations?
  seen_sites = site_info %>%
    filter(tags_det) %>%
    pull(site_code)
  unseen_sites = site_info %>%
    filter(!tags_det) %>%
    pull(site_code)


  #-----------------------------------
  # read in basic model file
  # open a connection
  mod_conn_init = file(init_file, open = 'r+')
  mod_file_org = readLines(mod_conn_init)
  close(mod_conn_init)
  rm(mod_conn_init)

  mod_file = mod_file_org
  # open a connection to new model file
  mod_conn_new = file(file_name, open = 'w')

  for(node in unseen_nodes) {
    mod_file[str_which(mod_file, paste0(node, "_p ~"))] = paste0('\t ', node, '_p <- 0; # no detections / not in operation')
  }

  if(length(unseen_nodes) > 0) {
    cat(paste('Fixed the following sites at 0% detection probability, because no tags were observed there:\n\t', paste(unseen_nodes, collapse = ', '), '.\n\n'))
  }

  #--------------------------

  # which sites have multiple nodes but only one had detections, or is a single node site, and no upstream sites?
  single_sites = node_info %>%
    group_by(site_code, n_nodes) %>%
    summarize(n_obs_nodes = sum(tags_det),
              obs_node = paste(node[tags_det], collapse = " "),
              .groups = "drop") %>%
    filter(n_obs_nodes > 0,
           (n_obs_nodes < n_nodes | n_nodes == 1)) %>%
    left_join(parent_child %>%
                group_by(site_code = parent) %>%
                summarize(n_child = n_distinct(child),
                          .groups = "drop"),
              by = "site_code") %>%
    mutate(across(n_child,
                  replace_na,
                  0)) %>%
    filter(n_child == 0,
           n_obs_nodes < 2)

  for(node in single_sites$obs_node) {
    mod_file[str_which(mod_file, paste0(node, "_p ~"))] = paste0('\t ', node, '_p <- 1; # Single array, no upstream detections')
  }

  if(nrow(single_sites) > 0) {
    cat(paste('Fixed the following nodes at 100% detection probability, because it is functioning as a single array with no upstream detections:\n\t', paste(single_sites$obs_node, collapse = ', '), '.\n\n'))
  }



  #--------------------------

  # if no observations at some terminal nodes, fix the movement probability past those nodes to 0
  if(sum(grepl('phi', mod_file)) > 0) {
    phi_0_nodes = tibble(mod_lines = mod_file[intersect(str_which(mod_file, "phi_"), str_which(mod_file, "~ dbeta"))]) %>%
      mutate(param = str_split(mod_lines, '\\~', simplify = T)[,1],
             param = str_remove(param, "^\\\t")) %>%
      select(param) %>%
      mutate(across(param,
                    str_trim)) %>%
      mutate(site_code = str_remove(param, "phi_"),
             site_code = str_split(site_code, "\\[", simplify = T)[,1]) %>%
      mutate(origin = str_extract(param, "[:digit:]") %>%
               as.numeric) %>%
      left_join(upstrm_nodes %>%
                  left_join(node_tags_origin %>%
                              mutate(origin = recode(origin,
                                                     "W" = 1,
                                                     "H" = 2)) %>%
                              rename(upstrm_node = node),
                            by = c("upstrm_node")) %>%
                  group_by(site_code, origin) %>%
                  summarise(upstrm_tags = sum(n_tags),
                            .groups = "drop"),
                by = c("site_code",
                       "origin")) %>%
      filter(upstrm_tags == 0) %>%
      mutate(mod_str = paste0("phi_", site_code, "\\[", origin, "\\]"))

    # str_which(mod_file, mod_str)

    if(nrow(phi_0_nodes) > 0) {

      for(i in 1:nrow(phi_0_nodes)) {
        mod_file[str_which(mod_file, paste0(phi_0_nodes$mod_str[i], " ~"))] = paste0('\t ', phi_0_nodes$param[i], ' <- 0 # no upstream detections')
      }

      cat(paste('\nFixed movement upstream of the following sites to 0 for at least one fish type because no detections upstream:\n\t', paste(unique(phi_0_nodes$site_code), collapse = ', '), '.\n\n'))
    }

  }

  writeLines(mod_file, mod_conn_new)
  close(mod_conn_new)

}
