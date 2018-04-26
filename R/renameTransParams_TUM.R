#' @title Rename DABOM Parameters - TUM
#'
#' @description Renames the various transition parameters in a matrix. This function is specific to the Tumwater version of DABOM.
#'
#' @author Kevin See
#'
#' @param param_vec A character vector of transition parameter names
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples renameTransParams_TUM()

renameTransParams_TUM = function(param_vec = NULL) {

  stopifnot(!is.null(param_vec))

  param_vec[grep('phi', param_vec)] = stringr::str_replace(param_vec[grep('phi', param_vec)], '\\[[[:digit:]]\\]', '')
  param_vec[grep('phi', param_vec)] = stringr::str_replace(toupper(param_vec[grep('phi', param_vec)]), 'PHI', 'past')
  param_vec[grep('p_pop_ICL', param_vec)] = c('ICL_bb', 'past_LNF', 'past_ICM')
  param_vec[grep('p_pop_TUM', param_vec)] = c('past_PES', 'past_ICL', 'past_CHW', 'past_CHL', 'past_NAL', 'past_WTL', 'past_LWN', 'TUM_bb')

  return(param_vec)

}
