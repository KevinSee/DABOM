#' @title PRA DABOM Initial Values
#'
#' @description Construct appropriate initial values for Priest Rapids version of DABOM
#'
#' @author Kevin See
#'
#' @param dabom_list output of \code{createDABOMcapHist} with parameter split_matrics set to \code{TRUE}.
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples setInitialValues_PRA()

setInitialValues_PRA = function(dabom_list = NULL) {

  stopifnot(!is.null(dabom_list))

  n.fish = nrow(dabom_list[[1]])
  n_branch_list = setBranchNums_PRA()

  # first lets create inits matrices
  a_list = vector('list', length(n_branch_list))
  names(a_list) = gsub('n_pops_', '', names(n_branch_list))
  for(i in 1:length(a_list)) {
    n_col = ifelse(names(a_list)[i] %in% c('PRA'),
                   n_branch_list[[i]],
                   n_branch_list[[i]] + 1)
    a_list[[i]] = array(0, dim = c(n.fish, n_col))
    rm(n_col)
  }


  # initial branching detects
  # Rock Island dam
  a_list[['PRA']][,2] = dabom_list[c('Wenatchee', 'Entiat', 'Methow', 'Okanogan')] %>%
    sapply(function(x) apply(x, 1, max, na.rm = T)) %>%
  apply(1, max)
  # Below Priest Rapids
  a_list[['PRA']][,3] = dabom_list$BelowPriest %>%
    apply(1, max)
  # initial black box
  a_list[['PRA']][,1] = abs(apply(a_list[['PRA']], 1, max, na.rm=T) - 1) #not seen anywhere

  # above Rock Island
  # not above Rock Island
  a_list[['RIA']][,ncol(a_list[['RIA']])] = abs(a_list$PRA[,2] - 1)
  # Clockum
  a_list[['RIA']][,2] = dabom_list$Wenatchee %>%
    select(matches("CLK")) %>%
    apply(1, max)
  # Wenatachee
  a_list[['RIA']][,3] = dabom_list$Wenatchee %>%
    select(-RIA, -matches('CLK')) %>%
    apply(1, max)
  # above Rocky Reach
  a_list[['RIA']][,4] = dabom_list[c('Entiat', 'Methow', 'Okanogan')] %>%
    sapply(function(x) apply(x, 1, max, na.rm = T)) %>%
    apply(1, max)
  # RIA bb
  a_list[['RIA']][,1] = ifelse(apply(a_list[['RIA']][,-1], 1, max) == 0,
                               1, 0)


  # above LWE
  # not there
  a_list[['LWE']][,ncol(a_list[['LWE']])] = abs(a_list$RIA[,3] - 1)
  # MCL
  a_list[['LWE']][,2] = dabom_list$Wenatchee %>%
    select(matches('MCL')) %>%
    apply(1, max)
  # PES
  a_list[['LWE']][,3] = dabom_list$Wenatchee %>%
    select(matches('PES'), matches('PEU')) %>%
    apply(1, max)

  z_peu_init = dabom_list$Wenatchee %>%
    select(matches('PEU')) %>%
    apply(1, max)

  # CHM
  a_list[['LWE']][,4] = dabom_list$Wenatchee %>%
    select(matches('CHM')) %>%
    apply(1, max)
  # ICL
  a_list[['LWE']][,5] = dabom_list$Wenatchee %>%
    select(matches('ICL'), matches('ICM'), matches('ICU'), matches('LNF')) %>%
    apply(1, max)
  # Tumwater
  a_list[['LWE']][,6] = dabom_list$Wenatchee %>%
    select(TUM:LWNA0) %>%
    apply(1, max)
  # LWE bb
  a_list[['LWE']][,1] = ifelse(apply(a_list[['LWE']][,-1], 1, max) == 0,
                               1, 0)

  # ICL
  # not there
  a_list[['ICL']][,ncol(a_list[['ICL']])] = abs(a_list$LWE[,5] - 1)
  # LNF
  a_list[['ICL']][,2] = dabom_list$Wenatchee %>%
    select(matches('LNF')) %>%
    apply(1, max)
  # ICM
  a_list[['ICL']][,3] = dabom_list$Wenatchee %>%
    select(matches('ICM'), matches('ICU')) %>%
    apply(1, max)
  z_icu_init = dabom_list$Wenatchee %>%
    select(matches('ICU')) %>%
    apply(1, max)
  # ICL bb
  a_list[['ICL']][,1] = ifelse(apply(a_list[['ICL']][,-1], 1, max) == 0,
                               1, 0)

  # Tumwater
  # not there
  a_list[['TUM']][,ncol(a_list[['TUM']])] = abs(a_list$LWE[,6] - 1)
  # CHW
  a_list[['TUM']][,2] = dabom_list$Wenatchee %>%
    select(matches('CHW')) %>%
    apply(1, max)
  # Chiwawa
  a_list[['TUM']][,3] = dabom_list$Wenatchee %>%
    select(matches('CHL'), matches('CHU')) %>%
    apply(1, max)
  z_chu_init = dabom_list$Wenatchee %>%
    select(matches('CHU')) %>%
    apply(1, max)
  # UWE
  a_list[['TUM']][,4] = dabom_list$Wenatchee %>%
    select(UWE:LWNA0) %>%
    apply(1, max)
  # TUM bb
  a_list[['TUM']][,1] = ifelse(apply(a_list[['TUM']][,-1], 1, max) == 0,
                               1, 0)

  # UWE
  # not there
  a_list[['UWE']][,ncol(a_list[['UWE']])] = abs(a_list$TUM[,4] - 1)
  # Nason
  a_list[['UWE']][,2] = dabom_list$Wenatchee %>%
    select(matches('NAL'), matches('NAU')) %>%
    apply(1, max)
  z_nau_init = dabom_list$Wenatchee %>%
    select(matches('NAU')) %>%
    apply(1, max)
  # WTL
  a_list[['UWE']][,3] = dabom_list$Wenatchee %>%
    select(matches('WTL')) %>%
    apply(1, max)
  # LWN
  a_list[['UWE']][,4] = dabom_list$Wenatchee %>%
    select(matches('LWN')) %>%
    apply(1, max)
  # UWE bb
  a_list[['UWE']][,1] = ifelse(apply(a_list[['UWE']][,-1], 1, max) == 0,
                               1, 0)


  # Rocky Reach
  # not there
  a_list[['RRF']][,ncol(a_list[['RRF']])] = abs(a_list$RIA[,4] - 1)
  # ENL
  a_list[['RRF']][,2] = dabom_list$Entiat %>%
    select(ENLB0:ENFA0) %>%
    apply(1, max)
  # above Wells
  a_list[['RRF']][,3] = dabom_list[c('Methow', 'Okanogan')] %>%
    sapply(function(x) apply(x, 1, max, na.rm = T)) %>%
    apply(1, max)
  # WVT
  a_list[['RRF']][,4] = dabom_list$Entiat %>%
    select(matches('WVT')) %>%
    apply(1, max)
  # RRF bb
  a_list[['RRF']][,1] = ifelse(apply(a_list[['RRF']][,-1], 1, max) == 0,
                               1, 0)


  # ENL
  # not there
  a_list[['ENL']][,ncol(a_list[['ENL']])] = abs(a_list$RRF[,2] - 1)
  # RCT
  a_list[['ENL']][,2] = dabom_list$Entiat %>%
    select(matches('RCT')) %>%
    apply(1, max)
  # EHL
  a_list[['ENL']][,3] = dabom_list$Entiat %>%
    select(matches('EHL')) %>%
    apply(1, max)
  # ENA
  a_list[['ENL']][,4] = dabom_list$Entiat %>%
    select(matches('ENA'), matches('ENM'), matches('ENS'), matches('ENF')) %>%
    apply(1, max)
  z_enm_init = dabom_list$Entiat %>%
    select(matches('ENM'), matches('ENS'), matches('ENF')) %>%
    apply(1, max)

  z_ens_init = dabom_list$Entiat %>%
    select(matches('ENS'), matches('ENF')) %>%
    apply(1, max)

  z_enf_init = dabom_list$Entiat %>%
    select(matches('ENF')) %>%
    apply(1, max)
  # MAD
  a_list[['ENL']][,5] = dabom_list$Entiat %>%
    select(matches('MAD')) %>%
    apply(1, max)
  # ENL bb
  a_list[['ENL']][,1] = ifelse(apply(a_list[['ENL']][,-1], 1, max) == 0,
                               1, 0)


  # above Wells
  # not there
  a_list[['WEA']][,ncol(a_list[['WEA']])] = abs(a_list$RRF[,3] - 1)
  # Methow
  a_list[['WEA']][,2] = dabom_list$Methow %>%
    select(-matches('WEA')) %>%
    apply(1, max)
  # Okanogan
  a_list[['WEA']][,3] = dabom_list$Okanogan %>%
    select(-matches('FST')) %>%
    apply(1, max)
  # FST
  a_list[['WEA']][,4] = dabom_list$Okanogan %>%
    select(matches('FST')) %>%
    apply(1, max)
  # WEA bb
  a_list[['WEA']][,1] = ifelse(apply(a_list[['WEA']][,-1], 1, max) == 0,
                               1, 0)

  # LMR
  # not there
  a_list[['LMR']][,ncol(a_list[['LMR']])] = abs(a_list$WEA[,2] - 1)
  # GLC
  a_list[['LMR']][,2] = dabom_list$Methow %>%
    select(matches('GLC')) %>%
    apply(1, max)
  # LBC
  a_list[['LMR']][,3] = dabom_list$Methow %>%
    select(matches('LBC')) %>%
    apply(1, max)
  # MRC
  a_list[['LMR']][,4] = dabom_list$Methow %>%
    select(MRCB0:WFC) %>%
    apply(1, max)
  # LMR bb
  a_list[['LMR']][,1] = ifelse(apply(a_list[['LMR']][,-1], 1, max) == 0,
                               1, 0)


  # MRC
  # not there
  a_list[['MRC']][,ncol(a_list[['MRC']])] = abs(a_list$LMR[,4] - 1)
  # BVC
  a_list[['MRC']][,2] = dabom_list$Methow %>%
    select(matches('BVC')) %>%
    apply(1, max)
  # TWR
  a_list[['MRC']][,3] = dabom_list$Methow %>%
    select(matches('TWR')) %>%
    apply(1, max)
  z_twispw_init = dabom_list$Methow %>%
    select(matches('TWISPW')) %>%
    apply(1, max)
  # CRW
  a_list[['MRC']][,4] = dabom_list$Methow %>%
    select(matches('CRW'), matches('CRU')) %>%
    apply(1, max)
  z_cru_init = dabom_list$Methow %>%
    select(matches('CRU')) %>%
    apply(1, max)
  # SCP
  a_list[['MRC']][,5] = dabom_list$Methow %>%
    select(matches('SCP')) %>%
    apply(1, max)
  # MSH
  a_list[['MRC']][,6] = dabom_list$Methow %>%
    select(matches('MSH'), matches('METH')) %>%
    apply(1, max)
  z_meth_init = dabom_list$Methow %>%
    select(matches('METH')) %>%
    apply(1, max)
  # MRW
  a_list[['MRC']][,7] = dabom_list$Methow %>%
    select(matches('MRW'), matches('WFC')) %>%
    apply(1, max)
  z_wfc_init = dabom_list$Methow %>%
    select(matches('WFC')) %>%
    apply(1, max)
  # MRC bb
  a_list[['MRC']][,1] = ifelse(apply(a_list[['MRC']][,-1], 1, max) == 0,
                               1, 0)



  # OKL
  # not there
  a_list[['OKL']][,ncol(a_list[['OKL']])] = abs(a_list$WEA[,3] - 1)
  # LLC
  a_list[['OKL']][,2] = dabom_list$Okanogan %>%
    select(matches('LLC')) %>%
    apply(1, max)
  # SA1
  a_list[['OKL']][,3] = dabom_list$Okanogan %>%
    select(matches('^SA1'), matches('^SA0')) %>%
    apply(1, max)
  z_sa0_init = dabom_list$Okanogan %>%
    select(matches('^SA0')) %>%
    apply(1, max)
  # OMK
  a_list[['OKL']][,4] = dabom_list$Okanogan %>%
    select(matches('OMK'), matches('OBF')) %>%
    apply(1, max)
  z_obf_init = dabom_list$Okanogan %>%
    select(matches('OBF')) %>%
    apply(1, max)
  # WAN
  a_list[['OKL']][,5] = dabom_list$Okanogan %>%
    select(matches('WAN')) %>%
    apply(1, max)
  # JOH
  a_list[['OKL']][,6] = dabom_list$Okanogan %>%
    select(matches('JOH')) %>%
    apply(1, max)
  # TNK
  a_list[['OKL']][,7] = dabom_list$Okanogan %>%
    select(matches('TNK')) %>%
    apply(1, max)
  # AEN
  a_list[['OKL']][,8] = dabom_list$Okanogan %>%
    select(matches('AEN')) %>%
    apply(1, max)
  # BPC
  a_list[['OKL']][,9] = dabom_list$Okanogan %>%
    select(matches('BPC')) %>%
    apply(1, max)
  # ANT
  a_list[['OKL']][,10] = dabom_list$Okanogan %>%
    select(matches('ANT')) %>%
    apply(1, max)
  # WHS
  a_list[['OKL']][,11] = dabom_list$Okanogan %>%
    select(matches('WHS')) %>%
    apply(1, max)
  # ZSL
  a_list[['OKL']][,12] = dabom_list$Okanogan %>%
    select(ZSLB0:OKCA0) %>%
    apply(1, max)
  # OKL bb
  a_list[['OKL']][,1] = ifelse(apply(a_list[['OKL']][,-1], 1, max) == 0,
                               1, 0)

  # ZSL
  # not there
  a_list[['ZSL']][,ncol(a_list[['ZSL']])] = abs(a_list$OKL[,12] - 1)
  # TON
  a_list[['ZSL']][,2] = dabom_list$Okanogan %>%
    select(matches('TON')) %>%
    apply(1, max)
  # NMC
  a_list[['ZSL']][,3] = dabom_list$Okanogan %>%
    select(matches('NMC')) %>%
    apply(1, max)
  # OKI
  a_list[['ZSL']][,4] = dabom_list$Okanogan %>%
    select(matches('OKI')) %>%
    apply(1, max)
  # OKC
  a_list[['ZSL']][,5] = dabom_list$Okanogan %>%
    select(matches('OKC')) %>%
    apply(1, max)
  # OKL bb
  a_list[['ZSL']][,1] = ifelse(apply(a_list[['ZSL']][,-1], 1, max) == 0,
                               1, 0)

  # Downstream of Prieset
  # not there
  a_list[['dwn']][,ncol(a_list[['dwn']])] = abs(a_list$PRA[,3] - 1)
  # below JD1
  a_list[['dwn']][,1] = dabom_list$BelowPriest %>%
    select(matches('BelowJD1')) %>%
    apply(1, max)
  # JD1
  a_list[['dwn']][,2] = dabom_list$BelowPriest %>%
    select(matches('^JD1')) %>%
    apply(1, max)
  # TMF
  a_list[['dwn']][,3] = dabom_list$BelowPriest %>%
    select(matches('TMF')) %>%
    apply(1, max)
  # PRV
  a_list[['dwn']][,4] = dabom_list$BelowPriest %>%
    select(matches('PRV'), matches('HST'), matches('MDR')) %>%
    apply(1, max)
  # ICH
  a_list[['dwn']][,5] = dabom_list$BelowPriest %>%
    select(matches('ICH')) %>%
    apply(1, max)
  # PRO
  a_list[['dwn']][,6] = dabom_list$BelowPriest %>%
    select(matches('PRO')) %>%
    apply(1, max)
  # RSH
  a_list[['dwn']][,7] = dabom_list$BelowPriest %>%
    select(matches('RSH')) %>%
    apply(1, max)
  # PRH
  a_list[['dwn']][,8] = dabom_list$BelowPriest %>%
    select(matches('PRH')) %>%
    apply(1, max)

  # PRV
  # not there
  a_list[['PRV']][,ncol(a_list[['PRV']])] = abs(a_list$dwn[,4] - 1)
  # HST
  a_list[['PRV']][,2] = dabom_list$BelowPriest %>%
    select(matches('HST')) %>%
    apply(1, max)
  # MDR
  a_list[['PRV']][,3] = dabom_list$BelowPriest %>%
    select(matches('MDR')) %>%
    apply(1, max)
  # downstream bb
  a_list[['PRV']][,1] = ifelse(apply(a_list[['PRV']][,-1], 1, max) == 0,
                               1, 0)

  # a_list %>%
  #   map_df(.f = function(x) {
  #     y = table(rowSums(x))
  #     return(ifelse(y['1'] == nrow(x), 'OK', 'Problem'))
  #   }) %>%
  #   gather(vector, res) %>%
  #   filter(res == 'Problem')


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
                      z_nau = z_nau_init,
                      z_enm = z_enm_init,
                      z_ens = z_ens_init,
                      z_enf = z_enf_init,
                      z_cru = z_cru_init,
                      z_meth = z_meth_init,
                      z_twispw = z_twispw_init,
                      z_wfc = z_wfc_init,
                      z_sa0 = z_sa0_init,
                      z_obf = z_obf_init),
                 as.vector))

    # y$a[y$a == n_branch_list$n.pops.main+1] = NA

    return(y)
  }

  return(jags.inits)
}
