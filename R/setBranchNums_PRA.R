#' @title Set Branch Numbers - Priest
#'
#' @description Returns list with the number of branches at each branching node in the DABOM model for Priest.
#'
#' @author Kevin See
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples setBranchNums_PRA()

setBranchNums_PRA = function() {

  n_branch_list = list(n_pops_PRA = 3,           # number of main branches
                       n_pops_RIA = 3,           # number of branches after RIA
                       n_pops_LWE = 6,
                       n_pops_ICL = 3,
                       n_pops_TUM = 4,
                       n_pops_UWE = 4,
                       n_pops_RRF = 4,
                       n_pops_ENL = 5,
                       n_pops_WEA = 4,
                       n_pops_LMR = 4,
                       n_pops_MRC = 7,
                       n_pops_OKL = 12,
                       n_pops_ZSL = 5,
                       n_pops_dwn = 8,
                       n_pops_PRV = 3)

}
