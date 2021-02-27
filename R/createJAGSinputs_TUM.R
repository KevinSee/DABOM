#' @title Prep DABOM JAGS inputs - TUM
#'
#' @description Construct all the necessary inputs to the DABOM JAGS model for TUM. Returns a named list that can be passed directly to JAGS.
#'
#' @author Kevin See
#'
#' @param dabom_list output of \code{createDABOMcapHist} with parameter split_matrics set to \code{TRUE}.
#' @param n_branch_list list containing an entry for each branching node, and an integer indicating the number of branches upstream of that node.
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples createJAGSinputs_TUM()

createJAGSinputs_TUM = function(dabom_list = NULL,
                                n_branch_list = NULL) {

  stopifnot(!is.null(dabom_list) |
              !is.null(n_branch_list))

  # set dirichlet vectors
  init_val_func = setInitialValues_TUM(dabom_list,
                                       n_branch_list)
  init_mats = init_val_func()

  dirich_input = vector('list', length(n_branch_list))
  for(i in 1:length(dirich_input)) {
    dirich_input[[i]]$n_brnch = n_branch_list[[i]]
    dirich_input[[i]]$w_tab = table(init_mats[[i]][dabom_list$fishOrigin == 1])
    dirich_input[[i]]$h_tab = table(init_mats[[i]][dabom_list$fishOrigin == 2])
    dirich_input[[i]]$nm = gsub('^a_', '', names(init_mats)[[i]])
  }

  dirich_vecs = purrr::map(dirich_input,
                           .f = function(x) {
                             dirich_mat = c(createDirichletVector(x$n_brnch,
                                                                  x$w_tab,
                                                                  initial_one = F,
                                                                  final_one = T),
                                            createDirichletVector(x$n_brnch,
                                                                  x$h_tab,
                                                                  initial_one = F,
                                                                  final_one = T)) %>%
                               matrix(nrow = 2,
                                      byrow = T)
                             return(dirich_mat)
                           })
  names(dirich_vecs) = paste0(gsub('^a_', '', names(init_mats)[grepl('^a_', names(init_mats))]), '_dirch_vec')


  jags_list = c(list(n_fish = nrow(dabom_list[[1]]),
                     # vector of zeros, large enough to match any element of dabom_list
                     zero_vec = rep(0, max(sapply(dabom_list, length)) + 1)),
                n_branch_list,
                dirich_vecs,
                dabom_list)

  return(jags_list)
}
