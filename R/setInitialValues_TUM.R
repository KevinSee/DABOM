#' @title Tumwater DABOM Initial Values
#'
#' @description Construct appropriate initial values for Tumwater version of DABOM
#'
#' @author Kevin See
#'
#' @param dabom_list output of \code{createDABOMcapHist} with parameter split_matrics set to \code{TRUE}.
#' @param model_file file path to JAGS text file.
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples setInitialValues_TUM()

setInitialValues_TUM = function(dabom_list = NULL,
                                model_file = NULL,
                                parent_child = NULL) {

  # how many tags?
  n_fish = nrow(dabom_list[[1]])

  # what node does model start with?
  root_node = parent_child %>%
    PITcleanr::buildNodeOrder() %>%
    filter(node_order == 1) %>%
    pull(node)

  n_branch_list = setBranchNums(parent_child) %>%
    # add a black box
    map(.f = function(x) x + 1)

  # add a "not there" bin for every branch node except root_node
  n_branch_list[!grepl(root_node, names(n_branch_list))] = n_branch_list[!grepl(root_node, names(n_branch_list))] %>%
    map(.f = function(x) x + 1)

  # read in JAGS model file
  mod_conn = file(model_file, open = 'rt')
  mod_file = readLines(mod_conn)
  close(mod_conn)
  rm(mod_conn)

  init_a = stringr::str_trim(mod_file[grep('a_', mod_file)])
  init_a = init_a[grepl('^a_', init_a)]
  init_a = init_a %>%
    stringr::str_split('\\[') %>%
    map_chr(.f = function(x) x[1])

  if(sum(!str_remove(init_a, '^a_') %in% names(n_branch_list)) > 0) {
    stop()
  }

  init_z = stringr::str_trim(mod_file[grep('z_', mod_file)])
  init_z = init_z[grepl('^z_', init_z)]
  init_z = init_z %>%
    stringr::str_split('\\[') %>%
    map_chr(.f = function(x) x[1])

  z_list = init_z %>%
    as.list() %>%
    rlang::set_names() %>%
    rlang::set_names(nm = str_remove(names(.), '^z_')) %>%
    map(.f = function(x) {
      rep(0, n_fish)
    })

  # first lets create inits matrices
  a_list = n_branch_list %>%
    purrr::map(.f = function(x) {
      array(0,
            dim = c(n_fish, x))
    })

  # initial branching detects
  for(i in 1:(ncol(a_list[['TUM']]) - 1)) {
    a_list[['TUM']][,i] = dabom_list[[i]] %>%
      select(-matches('UWE')) %>%
      apply(1, max)
  }
  # initial black box
  a_list[['TUM']][,8] = if_else(rowSums(a_list[["TUM"]]) == 0, 1, 0)

  # ICL
  # not there
  a_list[['ICL']][,ncol(a_list[['ICL']])] = abs(a_list[['TUM']][,2] - 1)
  # LNF / LEAV
  a_list[['ICL']][,1] = dabom_list$Icicle %>%
    select(matches('LNF')) %>%
    apply(1, max)
  # ICM
  a_list[['ICL']][,2] = dabom_list$Icicle %>%
    select(matches('ICM'), matches('ICU')) %>%
    apply(1, max)
  z_list[["ICU"]] = dabom_list$Icicle %>%
    select(matches('ICU')) %>%
    apply(1, max)
  # ICL bb
  a_list[["ICL"]][,3] = if_else(rowSums(a_list[["ICL"]]) == 0, 1, 0)

  # Peshastin
  z_list[["PEU"]] = dabom_list$Peshastin %>%
    select(matches('PEU')) %>%
    apply(1, max)

  # Chiwawa
  z_list[["CHU"]] = dabom_list$Chiwawa %>%
    select(matches('CHU')) %>%
    apply(1, max)

  # Nason
  z_list[["NAU"]] = dabom_list$Nason %>%
    select(matches('NAU')) %>%
    apply(1, max)

  # # do all the "a" matrices have a single 1 in every row?
  # a_list %>%
  #   map(.f = rowSums) %>%
  #   map(.f = table)

  # compile for JAGS
  names(a_list) = paste0('a_', names(a_list))
  names(z_list) = paste0('z_', names(z_list))

  jags.inits <- function() {
    y = c(purrr::map(a_list,
                     .f = function(x) {
                       x %*% seq(1, ncol(x)) %>%
                         as.vector
                     }),
          purrr::map(z_list,
                     .f = as.vector))
    return(y)
  }

  return(jags.inits)
}
