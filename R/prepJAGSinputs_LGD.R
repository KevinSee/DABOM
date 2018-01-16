#' @title Compile JAGS inputs - Lower Granite Dam
#'
#' @description Transforms data into appropriate format to feed into JAGS, including adding appropriate weekly strata for each tag if appropriate. Also constructs an initial value function to feed to JAGS. Currently this function is specific to the Lower Granite dam version of DABOM.
#'
#' @author Kevin See
#'
#' @inheritParams createDABOMcapHist
#' @param time_varying Should the initial movement probabilities be time-varying? Default value is \code{TRUE}
#' @param spawn_yr spawn year to divide into weekly strata
#' @param spp choices are either \code{Chinook} or \code{Steelhead}
#' @param start_day date (\code{month / day}) when strata should start
#' @param end_day date (\code{month / day}) when strata should end
#' @param strata_beg 3 letter code for the day of the week each weekly strata should begin on. Default value is \code{'Mon'}.
#' @param last_strata_min minimum length (in days) for the final strata. Default value is 3.
#'
#' @importFrom STADEM weeklyStrata
#' @import lubridate
#' @export
#' @return NULL
#' @examples \dontrun{prepJAGSinputs_LGD()}

prepJAGSinputs_LGD = function(proc_ch = NULL,
                          node_order = NULL,
                          time_varying = T,
                          spawn_yr = NULL,
                          spp = NULL,
                          start_day = NULL,
                          end_day = NULL,
                          strata_beg = 'Mon',
                          last_strata_min = 3) {

  if(is.null(spawn_yr)) stop('Year must be supplied')
  if(is.null(spp)) stop('Species must be supplied')


  dabom_df = createDABOMcapHist(proc_ch,
                                node_order,
                                split_matrices = F)

  dabom_list = createDABOMcapHist(proc_ch,
                                  node_order,
                                  split_matrices = T)


  # create a function to spit out initial values for MCMC chains
  init_fnc = setInitialValues_LGD(dabom_list)

  # pull together all data to feed to JAGS, in a named list
  jags_data = createJAGSinputs_LGD(dabom_list)

  if(time_varying) {
    week_strata = STADEM::weeklyStrata(spawn_yr = spawn_yr,
                                       spp = spp,
                                       start_day = start_day,
                                       end_day = end_day,
                                       strata_beg = strata_beg,
                                       last_strata_min = last_strata_min)

    dam_week = vector('integer', nrow(dabom_df))
    for(i in 1:length(week_strata)) {
      dam_week[which(dabom_df$TrapDate %within% week_strata[[i]])] = i
    }

    jags_data = c(jags_data,
                  list(n.weeks = length(week_strata),
                       dam_week = dam_week))

  }

  return(list(dabom_list = dabom_list,
              init_fnc = init_fnc,
              jags_data = jags_data))



}
