#' @title LGD DABOM Initial Values
#'
#' @description Construct appropriate initial values for Lower Granite version of DABOM
#'
#' @author Kevin See
#'
#' @param dabom_list output of \code{createDABOMcapHist} with parameter split_matrics set to \code{TRUE}.
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples setInitialValues_LGD()

setInitialValues_LGD = function(dabom_list = NULL) {

  stopifnot(!is.null(dabom_list))

  n.fish = nrow(dabom_list[[1]])
  n_branch_list = setBranchNums_LGD()

  # first lets create inits matrices
  a_init = array(0, dim=c(n.fish, n_branch_list$n.pops.main+1))
  a_Aso_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Asotin[1]+1))
  a_AsoUp_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Asotin[2]+1))
  a_Lap_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Lapwai+1))
  a_Pot_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Potlatch[1]+1))
  a_KHS_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Potlatch[2]+1))
  a_HLM_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Potlatch[3]+1))

  a_Imn_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Imnaha[1]+1))
  a_ImnUp_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Imnaha[2]+1))
  a_ImnWeir_init = array(0, dim = c(n.fish, n_branch_list$n.pops.Imnaha[3]+1))
  a_Wal_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Wallowa+1))
  a_UGR_init = array(0, dim=c(n.fish, n_branch_list$n.pops.UppGR+1))
  a_SFS_init = array(0, dim=c(n.fish, n_branch_list$n.pops.SFS+1))
  a_LowLem_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Lemhi[1]+1))
  a_UpLem_init = array(0, dim=c(n.fish, n_branch_list$n.pops.Lemhi[2]+1))
  a_UpSalm_init = array(0, dim=c(n.fish, n_branch_list$n.pops.UpSalm+1))


  # initial branching detects
  a_init[, 1:n_branch_list$n.pops.main] = sapply(dabom_list,
                                                 function(x) apply(x, 1, max, na.rm = T))
  # initial black box
  a_init[,n_branch_list$n.pops.main + 1] = abs(apply(a_init, 1, max, na.rm=T) - 1) #not seen anywhere


  # Tucannon
  z_mtr_init = dabom_list$Tucannon %>%
    select(MTR:TUCH_TFH) %>%
    apply(1, max)
  z_utr_init = dabom_list$Tucannon %>%
    select(UTR:TUCH_TFH) %>%
    apply(1, max)
  z_tuch_init = dabom_list$Tucannon %>%
    select(TUCH_TFH) %>%
    apply(1, max)

  # Asotin
  a_Aso_init[,ncol(a_Aso_init)] = 1 - apply(dabom_list$Asotin, 1, max, na.rm = T) # not in Asotin
  # GEORGC
  a_Aso_init[,2] = dabom_list$Asotin %>%
    select(GEORGC) %>%
    apply(1, max)
  # past ASOTIC
  a_Aso_init[,3] = dabom_list$Asotin %>%
    select(ASOTIC, ACB, CCAB0:AFCA0) %>%
    apply(1, max)
  # Asotin bb
  a_Aso_init[,1] = ifelse(apply(a_Aso_init[,-1], 1, max) == 0,
                          1, 0)

  z_acb_init = a_Aso_init[,3]

  # upper Asotin
  a_AsoUp_init[,ncol(a_AsoUp_init)] = abs(z_acb_init-1) # not in upper Asotin
  # CCA
  a_AsoUp_init[,2] = dabom_list$Asotin %>%
    select(matches('CCA')) %>%
    apply(1, max)
  # AFC
  a_AsoUp_init[,3] = dabom_list$Asotin %>%
    select(matches('AFC')) %>%
    apply(1, max)
  # upper Asotin bb
  a_AsoUp_init[,1] = ifelse(apply(a_AsoUp_init[,-1], 1, max) == 0,
                            1, 0)

  # Lapwai
  a_Lap_init[,ncol(a_Lap_init)] = 1 - apply(dabom_list$Lapwai, 1, max, na.rm = T) # not in Lapwai
  # SWT
  a_Lap_init[,2] = dabom_list$Lapwai %>%
    select(SWTB0:WEBA0) %>%
    apply(1,max)
  # MIS
  a_Lap_init[,3] = dabom_list$Lapwai %>%
    select(matches('MIS')) %>%
    apply(1,max)
  # Lapwai bb
  a_Lap_init[,1] = ifelse(apply(a_Lap_init[,-1], 1, max) == 0,
                          1, 0)
  # WEB
  z_web_init = dabom_list$Lapwai %>%
    select(matches('WEB')) %>%
    apply(1, max)

  # Potlatch
  a_Pot_init[,ncol(a_Pot_init)] = 1 - apply(dabom_list$Potlatch, 1, max, na.rm = T) # not in Potlatch
  # KHS
  a_Pot_init[,2] = dabom_list$Potlatch %>%
    select(BIGBEC:LBEARC) %>%
    apply(1, max)
  # PCM
  a_Pot_init[,3] = dabom_list$Potlatch %>%
    select(matches('PCM')) %>%
    apply(1, max)
  # HLM
  a_Pot_init[,4] = dabom_list$Potlatch %>%
    select(POTREF:POTRWF) %>%
    apply(1, max)
  # Potlatch bb
  a_Pot_init[,1] = ifelse(apply(a_Pot_init[,-1], 1, max) == 0,
                          1, 0)

  a_KHS_init[,ncol(a_KHS_init)] = abs(a_Pot_init[,2]-1) # not above KHS
  # BIGBEC
  a_KHS_init[,2] = dabom_list$Potlatch %>%
    select(BIGBEC) %>%
    apply(1, max)
  # LBEARC
  a_KHS_init[,3] = dabom_list$Potlatch %>%
    select(LBEARC) %>%
    apply(1, max)
  # KHS bb
  a_KHS_init[,1] = ifelse(apply(a_KHS_init[,-1], 1, max) == 0,
                          1, 0)

  a_HLM_init[,ncol(a_HLM_init)] = abs(a_Pot_init[,4]-1) # not above HLM
  # POTREF
  a_HLM_init[,2] = dabom_list$Potlatch %>%
    select(POTREF) %>%
    apply(1, max)
  # POTRWF
  a_HLM_init[,3] = dabom_list$Potlatch %>%
    select(POTRWF) %>%
    apply(1, max)
  # HLM bb
  a_HLM_init[,1] = ifelse(apply(a_HLM_init[,-1], 1, max) == 0,
                          1, 0)

  # Joseph Creek
  z_josepc_init = dabom_list$JosephCreek %>%
    select(JOSEPC) %>%
    apply(1, max)

  # Wallowa
  a_Wal_init[,ncol(a_Wal_init)] = 1 - apply(dabom_list$Wallowa, 1, max, na.rm = T) # not in Wallowa
  # BCANF
  a_Wal_init[,2] = dabom_list$Wallowa %>%
    select(BCANF) %>%
    apply(1, max)
  # LOSTIW
  a_Wal_init[,3] = dabom_list$Wallowa %>%
    select(LOSTIW) %>%
    apply(1, max)
  # WALH
  a_Wal_init[,4] = dabom_list$Wallowa %>%
    select(WALH) %>%
    apply(1, max)
  # Wallowa bb
  a_Wal_init[,1] = ifelse(apply(a_Wal_init[,-1], 1, max) == 0,
                          1, 0)

  # Grande Ronde
  a_UGR_init[,ncol(a_UGR_init)] = 1 - apply(dabom_list$GrandeRonde, 1, max, na.rm = T) # not in Grande Ronde
  # Catherine Creek
  a_UGR_init[,2] = dabom_list$GrandeRonde %>%
    select(CCWB0:CATHEW) %>%
    apply(1, max)
  # Grande Ronde weir
  a_UGR_init[,3] = dabom_list$GrandeRonde %>%
    select(GRANDW) %>%
    apply(1, max)
  # Grande Ronde bb
  a_UGR_init[,1] = ifelse(apply(a_UGR_init[,-1], 1, max) == 0,
                          1, 0)

  # Imnaha
  a_Imn_init[,ncol(a_Imn_init)] = 1 - apply(dabom_list$ImnahaRiver, 1, max, na.rm = T) # not in Imnaha
  # HORS3C
  a_Imn_init[,2] = dabom_list$ImnahaRiver %>%
    select(HORS3C) %>%
    apply(1, max)
  # CMP
  a_Imn_init[,3] = dabom_list$ImnahaRiver %>%
    select(matches('CMP')) %>%
    apply(1, max)
  # LSHEEF
  a_Imn_init[,4] = dabom_list$ImnahaRiver %>%
    select(LSHEEF) %>%
    apply(1, max)
  # BSC
  a_Imn_init[,5] = dabom_list$ImnahaRiver %>%
    select(matches('BSC')) %>%
    apply(1, max)
  # IR3
  a_Imn_init[,6] = dabom_list$ImnahaRiver %>%
    select(IR3B0:DRY2C) %>%
    apply(1, max)
  # Imnaha bb
  a_Imn_init[,1] = ifelse(apply(a_Imn_init[,-1], 1, max) == 0,
                          1, 0)

  a_ImnUp_init[,ncol(a_ImnUp_init)] = abs(a_Imn_init[,6]-1) # not in upper Imnaha
  # FREEZC
  a_ImnUp_init[,2] = dabom_list$ImnahaRiver %>%
    select(FREEZC) %>%
    apply(1, max)
  # CZY
  a_ImnUp_init[,3] = dabom_list$ImnahaRiver %>%
    select(matches('CZY')) %>%
    apply(1, max)
  # MAHOGC
  a_ImnUp_init[,4] = dabom_list$ImnahaRiver %>%
    select(MAHOGC) %>%
    apply(1, max)
  # IR4
  a_ImnUp_init[,5] = dabom_list$ImnahaRiver %>%
    select(IR4B0:DRY2C) %>%
    apply(1, max)
  # upper Imnaha bb
  a_ImnUp_init[,1] = ifelse(apply(a_ImnUp_init[,-1], 1, max) == 0,
                          1, 0)

  z_iml_init = dabom_list$ImnahaRiver %>%
    select(IMLB0:DRY2C) %>%
    apply(1, max)
  z_ir5_init = dabom_list$ImnahaRiver %>%
    select(IR5B0:DRY2C) %>%
    apply(1, max)

  a_ImnWeir_init[,ncol(a_ImnWeir_init)] = abs(z_ir5_init - 1) # not above Imnaha weir
  # GUMBTC
  a_ImnWeir_init[,2] = dabom_list$ImnahaRiver %>%
    select(GUMBTC) %>%
    apply(1, max)
  # DRY2C
  a_ImnWeir_init[,3] = dabom_list$ImnahaRiver %>%
    select(DRY2C) %>%
    apply(1, max)
  # Imnaha weir bb
  a_ImnWeir_init[,1] = ifelse(apply(a_ImnWeir_init[,-1], 1, max) == 0,
                              1, 0)

  # South Fork Salmon
  a_SFS_init[,ncol(a_SFS_init)] = 1 - apply(dabom_list$SFSalmon, 1, max, na.rm = T) # not in South Fork Salmon
  # ZEN
  a_SFS_init[,2] = dabom_list$SFSalmon %>%
    select(ZENB0:LAKEC) %>%
    apply(1, max)
  # ESS
  a_SFS_init[,3] = dabom_list$SFSalmon %>%
    select(ESSB0:JOHNSC) %>%
    apply(1, max)
  # KRS
  a_SFS_init[,4] = dabom_list$SFSalmon %>%
    select(KRS, STR) %>%
    apply(1, max)
  # SF Salmon bb
  a_SFS_init[,1] = ifelse(apply(a_SFS_init[,-1], 1, max) == 0,
                          1, 0)

  z_lakec_init = dabom_list$SFSalmon %>%
    select(LAKEC) %>%
    apply(1, max)
  z_johnsc_init = dabom_list$SFSalmon %>%
    select(JOHNSC) %>%
    apply(1, max)
  z_str_init = dabom_list$SFSalmon %>%
    select(STR) %>%
    apply(1, max)

  # Lower Lemhi
  a_LowLem_init[,ncol(a_LowLem_init)] = 1 - apply(dabom_list$Lemhi, 1, max, na.rm = T) # not in Lemhi
  # BHC
  a_LowLem_init[,2] = dabom_list$Lemhi %>%
    select(matches('BHC')) %>%
    apply(1, max)
  # WPC
  a_LowLem_init[,3] = dabom_list$Lemhi %>%
    select(matches('WPC')) %>%
    apply(1, max)
  # KEN
  a_LowLem_init[,4] = dabom_list$Lemhi %>%
    select(matches('KEN')) %>%
    apply(1, max)
  # AGC
  a_LowLem_init[,5] = dabom_list$Lemhi %>%
    select(matches('AGC')) %>%
    apply(1, max)
  # Hayden
  a_LowLem_init[,6] = dabom_list$Lemhi %>%
    select(matches('HYC'), HBC) %>%
    apply(1, max)
  # LRW
  a_LowLem_init[,7] = dabom_list$Lemhi %>%
    select(LRWB0:ncol(dabom_list$Lemhi)) %>%
    apply(1, max)
  # Lower Lemhi bb
  a_LowLem_init[,1] = ifelse(apply(a_LowLem_init[,-1], 1, max) == 0,
                             1, 0)

  # Upper Lemhi
  a_UpLem_init[,ncol(a_UpLem_init)] = abs(a_LowLem_init[,7]-1) # not in upper Lemhi
  # Little Springs
  a_UpLem_init[,2] = dabom_list$Lemhi %>%
    select(matches('LLS')) %>%
    apply(1, max)
  # Big Eightmile
  a_UpLem_init[,3] = dabom_list$Lemhi %>%
    select(matches('LB8')) %>%
    apply(1, max)
  # Big Springs
  a_UpLem_init[,4] = dabom_list$Lemhi %>%
    select(matches('LBS')) %>%
    apply(1, max)
  # Lee
  a_UpLem_init[,5] = dabom_list$Lemhi %>%
    select(matches('LCL')) %>%
    apply(1, max)
  # Big Timber
  a_UpLem_init[,6] = dabom_list$Lemhi %>%
    select(BTLB0:BTUA0) %>%
    apply(1, max)
  # Canyon
  a_UpLem_init[,7] = dabom_list$Lemhi %>%
    select(matches('CAC')) %>%
    apply(1, max)
  # 18 mile / Hawley
  a_UpLem_init[,8] = dabom_list$Lemhi %>%
    select(X18MB0:HECA0) %>%
    apply(1, max)
  # Upper Lemhi bb
  a_UpLem_init[,1] = ifelse(apply(a_UpLem_init[,-1], 1, max) == 0,
                            1, 0)

  # Upper Salmon
  a_UpSalm_init[,ncol(a_UpSalm_init)] = 1 - apply(dabom_list$UpperSalmon, 1, max, na.rm = T) # not in Upper Salmon
  # Pahsimeroi
  a_UpSalm_init[,2] = dabom_list$UpperSalmon %>%
    select(PAHH) %>%
    apply(1, max)
  # East Fork Salmon
  a_UpSalm_init[,3] = dabom_list$UpperSalmon %>%
    select(SALEFT) %>%
    apply(1, max)
  # Yankee Fork
  a_UpSalm_init[,4] = dabom_list$UpperSalmon %>%
    select(matches('YFK')) %>%
    apply(1, max)
  # Valley Creek
  a_UpSalm_init[,5] = dabom_list$UpperSalmon %>%
    select(VC2:VC1) %>%
    apply(1, max)
  # Sawtooth
  a_UpSalm_init[,6] = dabom_list$UpperSalmon %>%
    select(STL) %>%
    apply(1, max)
  # Upper Salmon bb
  a_UpSalm_init[,1] = ifelse(apply(a_UpSalm_init[,-1], 1, max) == 0,
                             1, 0)

  z_usi_init = dabom_list$UpperSalmon %>%
    select(-USE) %>%
    apply(1, max)


    # sapply(list(main = a_init,
    #             Pot = a_Pot_init,
    #             KHS = a_KHS_init,
    #             HLM = a_HLM_init,
    #             Lap = a_Lap_init,
    #             AsoLow = a_Aso_init,
    #             AsoUpp = a_AsoUp_init,
    #             ImnLow = a_Imn_init,
    #             ImnUpp = a_ImnUp_init,
    #             ImnWeir = a_ImnWeir_init,
    #             Wal = a_Wal_init,
    #             UGR = a_UGR_init,
    #             SFS = a_SFS_init,
    #             UpSalm = a_UpSalm_init,
    #             LemLow = a_LowLem_init,
    #             LemUpp = a_UpLem_init),
    #        function(x) {
    #          y = table(rowSums(x))
    #          return(ifelse(y['1'] == nrow(x), 'OK', 'Problem'))
    #          }) %>% unlist()

  jags.inits <- function() {
    c(lapply(list(a = a_init,
                  a_Aso = a_Aso_init,
                  a_AsoUp = a_AsoUp_init,
                  a_Lap = a_Lap_init,
                  a_Pot = a_Pot_init,
                  a_KHS = a_KHS_init,
                  a_HLM = a_HLM_init,
                  a_Imn = a_Imn_init,
                  a_ImnUp = a_ImnUp_init,
                  a_ImnWeir = a_ImnWeir_init,
                  a_Wal = a_Wal_init,
                  a_UGR = a_UGR_init,
                  a_SFS = a_SFS_init,
                  a_LowLem = a_LowLem_init,
                  a_UpLem = a_UpLem_init,
                  a_UpSalm = a_UpSalm_init),
             function(x) {
               x %*% seq(1, ncol(x)) %>%
                 as.vector
               }),
      lapply(list(z_mtr = z_mtr_init,
                  z_utr = z_utr_init,
                  z_tuch = z_tuch_init,
                  z_acb = z_acb_init,
                  z_web = z_web_init,
                  z_josepc = z_josepc_init,
                  z_iml = z_iml_init,
                  z_ir5 = z_ir5_init,
                  z_lakec = z_lakec_init,
                  z_johnsc = z_johnsc_init,
                  z_str = z_str_init,
                  z_usi = z_usi_init),
             as.vector))
  }

  return(jags.inits)
}
