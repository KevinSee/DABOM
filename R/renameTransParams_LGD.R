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

  param_vec[grep('phi', param_vec)] = stringr::str_replace(toupper(param_vec[grep('phi', param_vec)]), 'PHI_', '')
  param_vec[grep('p_pop_main', param_vec)] = c("LTR", "PENAWC", "ALMOTC", "ALPOWC", "ACM", "TENMC2", "LAP", "JUL", "JOC", "COC", "IR1", "LC1", "SC1", "WEN", "CLC", "LRL", "SW1", "LOOKGC", "WR1", "UGR", "RAPH", "SFG", "PCA", "TAY", "NFS", "CRC", "LLR", "USE","BRC", 'Main_bb')
  param_vec[grep('p_pop_Asotin', param_vec)] = c('ACM_bb', 'GEORGC', 'ASOTIC')
  param_vec[grep('p_pop_AsoUpp', param_vec)] = c('ACB_bb', 'CCA', 'AFC')
  param_vec[grep('p_pop_Lapwai', param_vec)] = c('LAP_bb', 'MIS', 'SWT')
  param_vec[grep('p_pop_Potlatch', param_vec)] = c('JUL_bb', 'KHS', 'PCM', 'HLM')
  param_vec[grep('p_pop_KHS', param_vec)] = c('KHS_bb', 'BIGBEC', 'LBEARC')
  param_vec[grep('p_pop_HLM', param_vec)] = c('HLM_bb', 'POTREF', 'POTRWF')
  param_vec[grep('p_pop_Imnaha', param_vec)] = c('IR1_bb', 'HORS3C', 'CMP', 'LSHEEF', 'BSC', 'IR3')
  param_vec[grep('p_pop_UppImn', param_vec)] = c('IR3_bb', 'FREEZC', 'CZY', 'MAHOGC', 'IR4')
  param_vec[grep('p_pop_ImnWeir', param_vec)] = c('IR5_bb', 'GUMBTC', 'DRY2C')
  param_vec[grep('p_pop_Wallowa', param_vec)] = c('WR1_bb', 'BCANF', 'WR2')
  param_vec[grep('p_pop_UppWall', param_vec)] = c('WR2_bb', 'LOSTIW', 'WALH')
  param_vec[grep('p_pop_UppGR', param_vec)] = c('UGR_bb', 'CATHEW', 'GRANDW')
  param_vec[grep('p_pop_SFS', param_vec)] = c('SFG_bb', 'ZEN', 'ESS', 'KRS')
  param_vec[grep('p_pop_ESS', param_vec)] = c('ESS_bb', 'JOHNSC', 'YPP')
  param_vec[grep('p_pop_LowLemhi', param_vec)] = c('LLR_bb', 'BHC', 'WPC', 'KEN', 'AGC', 'HYC', 'LRW')
  param_vec[grep('p_pop_UpLemhi', param_vec)] = c('LRW_bb', 'LLS', 'LB8', 'LBS', 'LCL', 'BTC', 'CAC', 'HEC')
  param_vec[grep('p_pop_UpSalm', param_vec)] = c('USI_bb', 'PAHH', 'SALEFT', 'YFK', 'VC2', 'RFL', 'STL')

  return(param_vec)

}
