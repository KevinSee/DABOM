#' @title Prep DABOM JAGS inputs
#'
#' @description Construct all the necessary inputs to the DABOM JAGS model. Returns a named list that can be passed directly to JAGS.
#'
#' @author Kevin See
#'
#' @inheritParams createDABOMcapHist
#' @param fish_origin tibble containing columns of `tag_code` and `origin`, where `origin` is either "W" for wild or "H" for hatchery
#'
#' @import dplyr rlang stringr
#' @export
#' @return NULL
#' @examples createJAGSinputs_PRA()

createJAGSinputs = function(filter_ch,
                            parent_child,
                            configuration,
                            fish_origin) {

  stopifnot(exprs = {
    !is.null(filter_ch)
    !is.null(parent_child)
    !is.null(configuration)
  })

  # determine starting point (root_site)
  root_site = PITcleanr::buildNodeOrder(parent_child) %>%
    filter(node_order == 1) %>%
    pull(node)

  # get the column names of the capture history matrix
  col_nms = defineDabomColNms(root_site = root_site,
                              parent_child = parent_child,
                              configuration = configuration) %>%
    unlist() %>%
    as.vector()

  # create capture history
  cap_hist = createDABOMcapHist(filter_ch = filter_ch,
                                parent_child = parent_child,
                                configuration = configuration,
                                split_matrices = F)

  # what kind of fish (wild or hatchery)
  fish_type = cap_hist %>%
    select(tag_code) %>%
    left_join(fish_origin %>%
                mutate(origin = recode(origin,
                                       "W" = 1,
                                       "H" = 2)),
              by = "tag_code") %>%
    pull(origin)

  # how many branches at each branching node?
  n_branch_list = setBranchNums(parent_child) %>%
    rlang::set_names(nm = function(x) paste0("n_branch_", x)) %>%
    # add a black box
    map(.f = function(x) x + 1)

  # set Dirichlet vectors
  init_val_func = setInitialValues(filter_ch = filter_ch,
                                   parent_child = parent_child,
                                   configuration = configuration)
  init_mats = init_val_func()

  # init_mats[stringr::str_remove(names(init_mats), '^a_') %in% stringr::str_remove(names(n_branch_list), 'n_branch_')]

  dirich_df = n_branch_list %>%
    unlist() %>%
    tibble::enframe(name = "site",
                    value = "n_brnch") %>%
    mutate(site = stringr::str_remove(site, 'n_branch_')) %>%
    left_join(init_mats %>%
                enframe(name = 'site',
                        value = 'inits') %>%
                mutate(site = stringr::str_remove(site, '^a_')),
              by = 'site') %>%
    mutate(dirch_vec = purrr::map2(n_brnch,
                                   inits,
                                   .f = function(x, y) {
                                     c(createDirichletVector(x,
                                                             table(y[fish_type == 1]),
                                                             initial_one = F,
                                                             final_one = T),
                                       createDirichletVector(x,
                                                             table(y[fish_type == 2]),
                                                             initial_one = F,
                                                             final_one = T)) %>%
                                       matrix(nrow = 2,
                                              byrow = T)
                                   }))

  dirich_vecs = dirich_df$dirch_vec %>%
    rlang::set_names(paste0(dirich_df$site, '_dirch_vec'))


  jags_list = c(list(n_fish = nrow(cap_hist),
                     # vector of zeros, large enough to match any element of dabom_list
                     zero_vec = rep(0, max(unlist(n_branch_list)) + 1),
                     cap_hist = cap_hist %>%
                       select(any_of(col_nms)),
                     fish_type = fish_type),
                n_branch_list,
                dirich_vecs)

  return(jags_list)


}
