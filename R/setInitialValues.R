#' @title DABOM Initial Values
#'
#' @description Construct appropriate initial values to be fed to JAGS. Focused on where tags were detected
#'
#' @author Kevin See
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr stringr tidyr rlang PITcleanr
#' @importFrom tibble enframe
#' @export
#' @return function
#' @examples setInitialValues()

setInitialValues = function(filter_ch,
                            parent_child,
                            configuration) {

  stopifnot(exprs = {
    !is.null(filter_ch)
    !is.null(parent_child)
    !is.null(configuration)
  })

  # how many child sites does each parent site have?
  parent_info = parent_child %>%
    group_by(parent, parent_rkm) %>%
    mutate(n_child = n_distinct(child))

  # determine parent site, and what branch number the tag must have taken
  branch_df = getNodeInfo(parent_child,
                          configuration) %>%
    select(site_code, parent_site, child_num) %>%
    distinct()

  # build node order
  no = parent_child %>%
    PITcleanr::addParentChildNodes(configuration = configuration) %>%
    PITcleanr::buildNodeOrder()

  # how many branches at each branching node?
  n_branch_list = setBranchNums(parent_child) %>%
    # add a black box
    map(.f = function(x) x + 1)

  # look at estimated spawn location, and the sites tag must have crossed to get there
  spawn_node = estimateSpawnLoc(filter_ch) %>%
    select(tag_code, spawn_node) %>%
    distinct() %>%
    mutate(spawn_site = if_else(grepl("_D$", spawn_node) &
                                  nchar(spawn_node) >= 5,
                                str_remove(spawn_node, "_D"),
                                spawn_node),
           spawn_site = if_else(grepl("_U$", spawn_site) &
                                  nchar(spawn_site) >= 5,
                                str_remove(spawn_site, "_U"),
                                spawn_site)) %>%
    left_join(no %>%
                select(spawn_node = node,
                       spawn_path = path),
              by = "spawn_node") %>%
    separate_rows(spawn_path) %>%
    rename(node = spawn_path) %>%
    left_join(no %>%
                select(node,
                       node_order),
              by = "node") %>%
    mutate(site_code = if_else(grepl("_D$", node) &
                                 nchar(node) >= 5,
                               str_remove(node, "_D"),
                               node),
           site_code = if_else(grepl("_U$", site_code) &
                                 nchar(site_code) >= 5,
                               str_remove(site_code, "_U"),
                               site_code)) %>%
    arrange(tag_code, node_order)


  # each tag passed each of these sites
  tag_sites = spawn_node %>%
    select(tag_code,
           spawn_site,
           site_code) %>%
    distinct() %>%
    group_by(tag_code) %>%
    mutate(lead_site = lead(site_code)) %>%
    ungroup() %>%
    left_join(branch_df %>%
                rename(lead_site = site_code,
                       site_code = parent_site),
              by = c("site_code", "lead_site")) %>%
    left_join(n_branch_list %>%
                unlist() %>%
                tibble::enframe(name = "site_code",
                                value = "max_branch"),
              by = "site_code") %>%
    mutate(child_num = if_else(is.na(child_num) & !is.na(max_branch),
                               as.integer(max_branch),
                               child_num))

  # construct some initial values lists
  a_list = tag_sites %>%
    filter(site_code %in% names(n_branch_list)) %>%
    split(list(.$site_code)) %>%
    map(.f = function(x) {
      not_there = max(x$max_branch) + 1

      x %>%
        complete(tag_code = unique(filter_ch$tag_code),
                 fill = list(child_num = not_there)) %>%
        arrange(tag_code) %>%
        select(tag_code, site_code, child_num) %>%
        pull(child_num)
    }) %>%
    rlang::set_names(nm = function(x) paste0("a_", x))

  # any eta initial values needed?
  n_eta <- parent_child %>%
    dplyr::count(parent,
                 name = "n_child") %>%
    filter(n_child == 1) %>%
    nrow()

  if(n_eta > 0) {
    eta_list = parent_child %>%
      dplyr::count(parent,
                   name = "n_child") %>%
      filter(n_child == 1) %>%
      select(parent) %>%
      split(list(.$parent)) %>%
      map(.f = function(x) {
        tag_sites %>%
          filter(site_code == x$parent) %>%
          mutate(seen = if_else(!is.na(lead_site),
                                1, 0)) %>%
          complete(tag_code = unique(filter_ch$tag_code),
                   fill = list(seen = 0)) %>%
          arrange(tag_code) %>%
          pull(seen)
      }) %>%
      rlang::set_names(nm = function(x) paste0("eta_", x))
  } else {
    eta_list = NULL
  }

  jags_inits <- function() {
    y = c(a_list,
          eta_list)
    return(y)
  }

  return(jags_inits)
}
