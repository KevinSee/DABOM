#' @title Tumwater DABOM Initial Values
#'
#' @description Construct appropriate initial values for Tumwater version of DABOM
#'
#' @author Kevin See
#'
#' @inheritParams createJAGSinputs_TUM
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples setInitialValues_TUM()

setInitialValues_TUM = function(dabom_list = NULL,
                                n_branch_list = NULL) {

  n.fish = nrow(dabom_list[[1]])

  # first lets create inits matrices
  a_list = vector('list', length(n_branch_list))
  names(a_list) = str_replace(names(n_branch_list), 'n_pops_', '')
  for(i in 1:length(a_list)) {
    n_col = ifelse(names(a_list)[i] %in% c('TUM'),
                   n_branch_list[[i]],
                   n_branch_list[[i]] + 1)
    a_list[[i]] = array(0, dim = c(n.fish, n_col))
    rm(n_col)
  }


  # initial branching detects
  for(i in 2:ncol(a_list[['TUM']])) {
    a_list[['TUM']][,i] = dabom_list[[i-1]] %>%
      apply(1, max)
  }
  # initial black box
  a_list[['TUM']][,1] = abs(apply(a_list[['TUM']], 1, max, na.rm=T) - 1) #not seen anywhere


  # ICL
  # not there
  a_list[['ICL']][,ncol(a_list[['ICL']])] = abs(a_list[['TUM']][,2] - 1)
  # LNF / LEAV
  a_list[['ICL']][,2] = dabom_list$Icicle %>%
    select(matches('LNF')) %>%
    apply(1, max)
  # ICM
  a_list[['ICL']][,3] = dabom_list$Icicle %>%
    select(matches('ICM'), matches('ICU')) %>%
    apply(1, max)
  z_icu_init = dabom_list$Icicle %>%
    select(matches('ICU')) %>%
    apply(1, max)
  # ICL bb
  a_list[['ICL']][,1] = ifelse(apply(a_list[['ICL']][,-1], 1, max) == 0,
                               1, 0)


  # Peshastin
  z_peu_init = dabom_list$Peshastin %>%
    select(matches('PEU')) %>%
    apply(1, max)

  # Chiwawa
  z_chu_init = dabom_list$Chiwawa %>%
    select(matches('CHU')) %>%
    apply(1, max)

  # Nason
  z_nau_init = dabom_list$Nason %>%
    select(matches('NAU')) %>%
    apply(1, max)

  names(a_list) = paste0('a_', names(a_list))

  jags.inits <- function() {
    y = c(purrr::map(a_list,
                     .f = function(x) {
                       x %*% seq(1, ncol(x)) %>%
                         as.vector
                     }),
          lapply(list(z_peu = z_peu_init,
                      z_icu = z_icu_init,
                      z_chu = z_chu_init,
                      z_nau = z_nau_init),
                 as.vector))

    # y$a[y$a == n_branch_list$n.pops.main+1] = NA

    return(y)
  }

  return(jags.inits)
}
