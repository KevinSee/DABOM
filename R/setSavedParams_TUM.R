#' @title Set Saved Parameters - TUM
#'
#' @description Returns a character vector of parameters to be saved by JAGS in the Tumwater version of DABOM
#'
#' @author Kevin See
#'
#' @param time_varying Should the initial movement probabilities be time-varying? Default value is \code{FALSE}
#'
#' @export
#' @return NULL
#' @examples setSavedParams_TUM()

setSavedParams_TUM = function(time_varying = F) {

  # start with movement probabilities
  my_params = c('p_pop_TUM',
                'p_pop_ICL',
                'phi_icu',
                'phi_peu',
                'phi_chu',
                'phi_nau')

  # add detection probabilities
  my_params = c(my_params,
                'PESB0_p',
                'PESA0_p',
                'PEUB0_p',
                'PEUA0_p',
                'ICLB0_p',
                'ICLA0_p',
                'LNFB0_p',
                'LNFA0_p',
                'ICMB0_p',
                'ICMA0_p',
                'ICUB0_p',
                'ICUA0_p',
                'CHWB0_p',
                'CHWA0_p',
                'CHLB0_p',
                'CHLA0_p',
                'CHUB0_p',
                'CHUA0_p',
                'UWE_p',
                'NALB0_p',
                'NALA0_p',
                'NAUB0_p',
                'NAUA0_p',
                'WTLB0_p',
                'WTLA0_p',
                'LWNB0_p',
                'LWNA0_p')

  if(time_varying) {
    my_params = c(my_params,
                  'sigma_rw')
  }

  return(my_params)
}
