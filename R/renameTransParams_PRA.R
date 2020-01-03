#' @title Rename DABOM Parameters - PRA
#'
#' @description Renames the various transition parameters in a matrix. This function is specific to the Priest Rapids version of DABOM.
#'
#' @author Kevin See
#'
#' @param param_vec A character vector of transition parameter names
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples renameTransParams_PRA()

renameTransParams_PRA = function(param_vec = NULL) {

  stopifnot(!is.null(param_vec))

  param_vec[grep('phi', param_vec)] = stringr::str_replace(param_vec[grep('phi', param_vec)], '\\[[[:digit:]]\\]', '')
  param_vec[grep('phi', param_vec)] = stringr::str_replace(toupper(param_vec[grep('phi', param_vec)]), 'PHI', 'past')
  param_vec[grep('p_pop_PRA', param_vec)] = c('PRA_bb', 'past_RIA', 'dwnStrm')
  param_vec[grep('p_pop_RIA', param_vec)] = c('RIA_bb', 'past_CLK', 'past_LWE', 'past_RRF')
  param_vec[grep('p_pop_LWE', param_vec)] = c('LWE_bb', 'past_MCL', 'past_PES', 'past_CHM', 'past_ICL', 'past_TUM')
  param_vec[grep('p_pop_ICL', param_vec)] = c('ICL_bb', 'past_LNF', 'past_ICM')
  param_vec[grep('p_pop_TUM', param_vec)] = c('TUM_bb', 'past_CHW', 'past_CHL', 'past_UWE')
  param_vec[grep('p_pop_UWE', param_vec)] = c('UWE_bb', 'past_NAL', 'past_WTL', 'past_LWN')
  param_vec[grep('p_pop_RRF', param_vec)] = c('RRF_bb', 'past_ENL', 'past_WEA', 'past_WVT')
  param_vec[grep('p_pop_ENL', param_vec)] = c('ENL_bb', 'past_RCT', 'past_EHL', 'past_ENA', 'past_MAD')
  param_vec[grep('p_pop_WEA', param_vec)] = c('WEA_bb', 'past_LMR', 'past_OKL', 'past_FST')
  param_vec[grep('p_pop_LMR', param_vec)] = c('LMR_bb', 'past_GLC', 'past_LBC', 'past_MRC')
  param_vec[grep('p_pop_MRC', param_vec)] = c('MRC_bb', 'past_BVC', 'past_TWR', 'past_CRW', 'past_SCP', 'past_MSH', 'past_MRW')
  param_vec[grep('p_pop_OKL', param_vec)] = c('OKL_bb', 'past_LLC', 'past_SA1', 'past_OMK', 'past_WAN', 'past_JOH', 'past_TNK', 'past_AEN', 'past_BPC', 'past_ANT', 'past_WHS', 'past_ZSL')
  param_vec[grep('p_pop_ZSL', param_vec)] = c('ZSL_bb', 'past_TON', 'past_NMC', 'past_OKI', 'past_OKC')
  param_vec[grep('p_pop_dwn', param_vec)] = c('BelowJD1', 'past_JD1', 'past_TMF', 'past_PRV', 'past_ICH', 'past_PRO', 'past_RSH', 'past_PRH')
  param_vec[grep('p_pop_PRV', param_vec)] = c('PRV_bb', 'past_HST', 'past_MDR')

  return(param_vec)

}
