#' @title Add Time-Varying Data
#'
#' @description Adds additional data required for the time-varying version of DABOM.
#'
#' @author Kevin See
#'
#' @param proc_ch capture history as returned by one of the \code{processCapHist} family of functions in \code{PITcleanr} package, which has then been verified by a user and all blank UserProcStatus entries have been completed.
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
#' @examples addTimeVaryData()

addTimeVaryData = function(proc_ch = NULL,
                           spawn_yr,
                           spp = c('Chinook', 'Steelhead'),
                           start_day = NULL,
                           end_day = NULL,
                           strata_beg = 'Mon',
                           last_strata_min = 3) {

  stopifnot(!is.null(proc_ch) |
              !is.null(spawn_yr) |
              !is.null(spp))

  week_strata = STADEM::weeklyStrata(spawn_yr = spawn_yr,
                                     spp = spp,
                                     start_day = start_day,
                                     end_day = end_day,
                                     strata_beg = strata_beg,
                                     last_strata_min = last_strata_min)

  lgr_week = vector('integer', nrow(proc_ch))
  for(i in 1:length(week_strata)) {
    lgr_week[which(proc_ch$TrapDate %within% week_strata[[i]])] = i
  }

  tv_list = list(n.weeks = length(week_strata),
                 lgr_week = lgr_week)

  return(tv_list)

}
