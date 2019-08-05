#' @title Set Branch Numbers - Lower Granite
#'
#' @description Returns list with the number of branches at each branching node in the DABOM model for Lower Granite.
#'
#' @author Kevin See
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples setBranchNums_LGD()

setBranchNums_LGD = function() {

  n_branch_list = list(n.pops.main = 30,           # number of main branches
                       n.pops.Asotin = c(3,3),     # number of branches after ACM, then number of branches after ACB
                       n.pops.Lapwai = 3,				   # number of bins in Lapwai branch
                       n.pops.Potlatch = c(4,3,3), # number of bins in Potlatch branch, then above KHS, then above HLM
                       n.pops.Wallowa = c(3,3),         # number of initial bins in Wallowa branch
                       n.pops.UppGR = 3,           # number of initial bins in Grande Ronde branch
                       n.pops.Imnaha = c(6,5,3),   # number of initial bins in Imnaha branch, number of branches above IR3, number of branches above IR5
                       n.pops.SFS = c(4,3),			       # number of initial bins in South Fork Salmon branch
                       n.pops.Lemhi = c(7,8),		   # number of bins in lower and upper Lemhi branches
                       n.pops.UpSalm = 7)          # number of initial bins in Upper Salmon branch

}
