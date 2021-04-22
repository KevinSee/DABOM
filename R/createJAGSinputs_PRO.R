#' @title Prep DABOM JAGS inputs - PRO
#'
#' @description Construct all the necessary inputs to the DABOM JAGS model for PRO. Returns a named list that can be passed directly to JAGS.
#'
#' @author Kevin See
#'
#' @inheritParams setInitialValues_PRO
#'
#' @import dplyr rlang stringr
#' @export
#' @return NULL
#' @examples createJAGSinputs_PRA()

createJAGSinputs_PRO = function(dabom_list = NULL,
                                model_file = NULL,
                                parent_child = NULL) {

  stopifnot(!is.null(dabom_list))

  # how many branches at each branching node?
  n_branch_list = setBranchNums(parent_child) %>%
    # add a black box
    map(.f = function(x) x + 1)

  # set dirichlet vectors
  init_val_func = setInitialValues_PRO(dabom_list,
                                       model_file,
                                       parent_child)
  init_mats = init_val_func()

  init_mats[stringr::str_remove(names(init_mats), '^a_') %in% stringr::str_remove(names(n_branch_list), 'n_pops_')]

  dirich_df = n_branch_list %>%
    map_df(.id = 'site',
           .f = function(x) tibble(n_brnch = x)) %>%
    mutate(site = stringr::str_remove(site, 'n_pops_')) %>%
    left_join(init_mats %>%
                enframe(name = 'site',
                        value = 'inits') %>%
                mutate(site = stringr::str_remove(site, '^a_')),
              by = 'site') %>%
    mutate(dirch_vec = purrr::map2(n_brnch,
                                   inits,
                                   .f = function(x, y) {
                                     c(createDirichletVector(x,
                                                           table(y[dabom_list$fishOrigin == 1]),
                                                           initial_one = F,
                                                           final_one = T),
                                       createDirichletVector(x,
                                                             table(y[dabom_list$fishOrigin == 2]),
                                                             initial_one = F,
                                                             final_one = T)) %>%
                                       matrix(nrow = 2,
                                              byrow = T)
                                   }))

  dirich_vecs = dirich_df$dirch_vec %>%
    rlang::set_names(paste0(dirich_df$site, '_dirch_vec'))


  jags_list = c(list(n_fish = nrow(dabom_list[[1]]),
                     # vector of zeros, large enough to match any element of dabom_list
                     zero_vec = rep(0, max(purrr::map_int(dabom_list[!grepl('fishOrigin', names(dabom_list))], length)) + 1)),
                n_branch_list,
                dirich_vecs,
                dabom_list)

  return(jags_list)
}
