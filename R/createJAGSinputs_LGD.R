#' @title Prep DABOM JAGS inputs
#'
#' @description Construct all the necessary inputs to the DABOM JAGS model for LGR. Returns a named list that can be passed directly to JAGS.
#'
#' @author Kevin See
#'
#' @param dabom_list output of \code{createDABOMcapHist} with parameter split_matrics set to \code{TRUE}.
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples createJAGSinputs_LGD()

createJAGSinputs_LGD = function(dabom_list = NULL) {

  stopifnot(!is.null(dabom_list))

  # how many branches at each branching node?
  n_branch_list = setBranchNums_LGD()

  # set dirichlet vectors
  init_val_func = setInitialValues_LGD(dabom_list)
  init_mats = init_val_func()

  dirich_vecs = list(main_dirch_vec = createDirichletVector(n_branch_list$n.pops.main,
                                                            table(init_mats$a),
                                                            initial_one = F,
                                                            final_one = T),
                     aso_dirch_vec = createDirichletVector(n_branch_list$n.pops.Asotin[1],
                                                           table(init_mats$a_Aso)),
                     asoUp_dirch_vec = createDirichletVector(n_branch_list$n.pops.Asotin[2],
                                                             table(init_mats$a_AsoUp)),
                     lap_dirch_vec = createDirichletVector(n_branch_list$n.pops.Lapwai,
                                                           table(init_mats$a_Lap)),
                     pot_dirch_vec = createDirichletVector(n_branch_list$n.pops.Potlatch[1],
                                                           table(init_mats$a_Pot)),
                     khs_dirch_vec = createDirichletVector(n_branch_list$n.pops.Potlatch[2],
                                                           table(init_mats$a_KHS)),
                     hlm_dirch_vec = createDirichletVector(n_branch_list$n.pops.Potlatch[3],
                                                           table(init_mats$a_HLM)),
                     wal_dirch_vec = createDirichletVector(n_branch_list$n.pops.Wallowa,
                                                           table(init_mats$a_Wal)),
                     ugr_dirch_vec = createDirichletVector(n_branch_list$n.pops.UppGR,
                                                           table(init_mats$a_UGR)),
                     imn_dirch_vec = createDirichletVector(n_branch_list$n.pops.Imnaha[1],
                                                           table(init_mats$a_Imn)),
                     imnUp_dirch_vec = createDirichletVector(n_branch_list$n.pops.Imnaha[2],
                                                             table(init_mats$a_ImnUp)),
                     imnWeir_dirch_vec = createDirichletVector(n_branch_list$n.pops.Imnaha[3],
                                                               table(init_mats$a_ImnWeir)),
                     sfs_dirch_vec = createDirichletVector(n_branch_list$n.pops.SFS,
                                                           table(init_mats$a_SFS)),
                     lemlow_dirch_vec = createDirichletVector(n_branch_list$n.pops.Lemhi[1],
                                                              table(init_mats$a_LowLem)),
                     lemupp_dirch_vec = createDirichletVector(n_branch_list$n.pops.Lemhi[2],
                                                              table(init_mats$a_UpLem)),
                     upsalm_dirch_vec = createDirichletVector(n_branch_list$n.pops.UpSalm,
                                                              table(init_mats$a_UpSalm)))


  jags_list = c(list(n.fish = nrow(dabom_list[[1]]),
                     # vector of zeros, large enough to match any element of dabom_list
                     zero_vec = map_int(dabom_list,
                                        .f = length) %>%
                       max() %>%
                       rep(0, .) %>%
                       c(.,0)),
                n_branch_list,
                dirich_vecs,
                dabom_list)

  return(jags_list)
}
