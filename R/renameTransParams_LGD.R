#' @title Rename DABOM Parameters - LGD
#'
#' @description Renames the various transition parameters in a matrix. This function is specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param param_vec A character vector of transition parameter names
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples renameTransParams_LGD()

renameTransParams_LGD = function(param_vec = NULL) {

  stopifnot(!is.null(param_vec))

  param_vec[grep('phi', param_vec)] = stringr::str_replace(toupper(param_vec[grep('phi', param_vec)]), 'PHI', 'past')
  param_vec[grep('p_pop_main', param_vec)] = c("Tucannon", "Penawawa", "Almota", "Alpowa", "Asotin", "TenMileCreek", "Lapwai", "Potlatch", "JosephCreek", "CowCreek", "ImnahaRiver", "Lolo", "SFClearwater", "Wenaha", "ClearCreek", "Lochsa", "Selway", "LookingGlass", "Wallowa", "GrandeRonde", "RapidRiver", "SFSalmon", "Panther", "BigCreek", "NFSalmon", "CarmenCreek", "Lemhi", "UpperSalmon","BearValley", 'Main_bb')
  param_vec[grep('p_pop_Asotin', param_vec)] = c('Asotin_bb', 'GEORGC', 'past_ASOTIC')
  param_vec[grep('p_pop_AsoUpp', param_vec)] = c('ACB_bb', 'past_CCA', 'past_AFC')
  param_vec[grep('p_pop_Lapwai', param_vec)] = c('Lapwai_bb', 'past_MIS', 'past_SWT')
  param_vec[grep('p_pop_Potlatch', param_vec)] = c('Potlatch_bb', 'past_KHS', 'past_PCM', 'past_HLM')
  param_vec[grep('p_pop_KHS', param_vec)] = c('KHS_bb', 'BIGBEC', 'LBEARC')
  param_vec[grep('p_pop_HLM', param_vec)] = c('HLM_bb', 'POTREF', 'POTRWF')
  param_vec[grep('p_pop_Imnaha', param_vec)] = c('ImnahaRiver_bb', 'HORS3C', 'past_CMP', 'LSHEEF', 'past_BSC', 'past_IR3')
  param_vec[grep('p_pop_UppImn', param_vec)] = c('IR3_bb', 'FREEZC', 'past_CZY', 'MAHOGC', 'past_IR4')
  param_vec[grep('p_pop_ImnWeir', param_vec)] = c('IR5_bb', 'GUMBTC', 'DRY2C')
  param_vec[grep('p_pop_Wallowa', param_vec)] = c('Wallowa_bb', 'BCANF', 'past_WR2')
  param_vec[grep('p_pop_UppWall', param_vec)] = c('WR2_bb', 'LOSTIW', 'WALH')
  param_vec[grep('p_pop_UppGR', param_vec)] = c('GrandeRonde_bb', 'CATHEW', 'GRANDW')
  param_vec[grep('p_pop_SFS', param_vec)] = c('SFSalmon_bb', 'past_ZEN', 'past_ESS', 'past_KRS')
  param_vec[grep('p_pop_LowLemhi', param_vec)] = c('Lemhi_bb', 'past_BHC', 'past_WPC', 'past_KEN', 'past_AGC', 'past_HYC', 'past_LRW')
  param_vec[grep('p_pop_UpLemhi', param_vec)] = c('LRW_bb', 'past_LLS', 'past_LB8', 'past_LBS', 'past_LCL', 'past_BTC', 'past_CAC', 'past_HEC')
  param_vec[grep('p_pop_UpSalm', param_vec)] = c('USI_bb', 'PAHH', 'SALEFT', 'past_YFK', 'past_VC2', 'past_RFL', 'past_STL')

  return(param_vec)

}
