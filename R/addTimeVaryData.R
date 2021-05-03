#' @title Add Time-Varying Data
#'
#' @description Adds additional data required for the time-varying version of DABOM.
#'
#' @author Kevin See
#'
#' @inheritParams createDABOMcapHist
#' @param start_date character vector of date (\code{YYYYMMDD}) when query should start
#' @param end_date character vector of date (\code{YYYYMMDD}) when query should end
#' @param strata_beg 3 letter code for the day of the week each weekly strata should begin on. Default value is \code{'Mon'}.
#' @param last_strata_min minimum length (in days) for the final strata. Default value is 3.

#'
#' @importFrom STADEM weeklyStrata
#' @import lubridate
#' @export
#' @return NULL
#' @examples addTimeVaryData()

addTimeVaryData = function(filter_ch = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           strata_beg = 'Mon',
                           last_strata_min = 3) {

  stopifnot(!is.null(filter_ch))

  if(is.null(start_date)) start_date = min(filter_ch$start_date, na.rm = T) %>%
      format('%Y%m%d')

  if(is.null(end_date)) end_date = max(filter_ch$start_date, na.rm = T) %>%
      format('%Y%m%d')

  week_strata = STADEM::weeklyStrata(start_date = start_date,
                                     end_date = end_date,
                                     strata_beg = strata_beg,
                                     last_strata_min = last_strata_min)

  proc_trap_date = filter_ch %>%
    select(tag_code, start_date) %>%
    distinct()

  if(nrow(proc_trap_date) != n_distinct(filter_ch$tag_code)) {
    stop('Multiple trap dates for some tags')
  }

  dam_week = vector('integer', nrow(proc_trap_date))
  for(i in 1:length(week_strata)) {
    dam_week[which(proc_trap_date$start_date %within% week_strata[[i]])] = i
  }

  tv_list = list(n_weeks = length(week_strata),
                 dam_week = dam_week)

  return(tv_list)

}
