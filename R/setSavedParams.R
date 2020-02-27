#' @title Set Saved Parameters - LGD
#'
#' @description Returns a character vector of parameters to be saved by JAGS in the Lower Granite version of DABOM
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
  branch_params = stringr::str_trim(mod_file[grep('p_pop', mod_file)])
  branch_params = branch_params[grep('^p_pop', branch_params)]
  branch_params = branch_params %>%
    stringr::str_split('\\[') %>%
    purrr::map_chr(.f = function(x) x[1]) %>%
    unique()

  phi_params = stringr::str_trim(mod_file[grep('phi_', mod_file)])
  phi_params = phi_params[grep('^phi_', phi_params)]
  phi_params = phi_params %>%
    stringr::str_split('\\[') %>%
    purrr::map_chr(.f = function(x) x[1]) %>%
    unique()

  my_params = c(branch_params,
                phi_params,
                det_params)

  if(time_varying) {
    my_params = c(my_params,
                  'sigma_rw')
  }

  return(my_params)
}
