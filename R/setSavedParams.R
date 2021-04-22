#' @title Set Saved Parameters
#'
#' @description Returns a character vector of parameters to be saved by JAGS for a DABOM model
#'
#' @author Kevin See
#'
#' @param model_file file path to the JAGS model text file
#' @param time_varying Should the initial movement probabilities be time-varying? Default value is \code{FALSE}
#'
#' @import dplyr stringr purrr
#' @export
#' @return NULL
#' @examples setSavedParams()

setSavedParams = function(model_file = NULL,
                          time_varying = F) {


  mod_conn = file(model_file, open = 'r+')
  mod_file = readLines(mod_conn)
  close(mod_conn)
  rm(mod_conn)

  # detection probabilities
  mod_nodes = c(stringr::str_trim(mod_file[grep('_p ~', mod_file)]),
                stringr::str_trim(mod_file[grep('_p <-', mod_file)])) %>%
    stringr::str_split(pattern = '_p') %>%
    purrr::map_chr(.f = function(x) x[1])

  det_params = paste0(mod_nodes, '_p')

  # movement probabilities
  # newer versions use these types of parameter names
  psi_params = stringr::str_trim(mod_file[grep('psi_', mod_file)])
  psi_params = psi_params[grep('^psi_', psi_params)]
  psi_params = psi_params %>%
    stringr::str_split('\\[') %>%
    purrr::map_chr(.f = function(x) x[1]) %>%
    unique()

  # older versions use these types of parameter names
  branch_params = stringr::str_trim(mod_file[grep('p_pop', mod_file)])
  branch_params = branch_params[grep('^p_pop', branch_params)]
  branch_params = branch_params %>%
    stringr::str_split('\\[') %>%
    purrr::map_chr(.f = function(x) x[1]) %>%
    unique()

  # all versions use these types of parameter names
  phi_params = stringr::str_trim(mod_file[grep('phi_', mod_file)])
  phi_params = phi_params[grep('^phi_', phi_params)]
  phi_params = phi_params %>%
    stringr::str_split('\\[') %>%
    purrr::map_chr(.f = function(x) x[1]) %>%
    unique()

  my_params = c(branch_params,
                psi_params,
                phi_params,
                det_params)

  if(time_varying) {
    my_params = c(my_params,
                  'sigma_rw')
  }

  return(my_params)
}
