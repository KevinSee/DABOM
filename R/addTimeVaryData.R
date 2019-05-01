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

addTimeVaryData = function(proc_ch = NULL,
                           node_order = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           strata_beg = 'Mon',
                           last_strata_min = 3) {

  stopifnot(!is.null(proc_ch))

  if(is.null(start_date)) start_date = min(proc_ch$TrapDate, na.rm = T) %>%
      format('%Y%m%d')

  if(is.null(end_date)) end_date = max(proc_ch$TrapDate, na.rm = T) %>%
      format('%Y%m%d')

  week_strata = STADEM::weeklyStrata(start_date = start_date,
                                     end_date = end_date,
                                     strata_beg = strata_beg,
                                     last_strata_min = last_strata_min)

  proc_trap_date = createDABOMcapHist(proc_ch,
                                      node_order,
                                      split_matrices = F) %>%
    select(TagID, TrapDate) %>%
    distinct()

  if(nrow(proc_trap_date) != n_distinct(proc_ch$TagID)) {
    stop('Multiple trap dates for some tags')
  }

  dam_week = vector('integer', nrow(proc_trap_date))
  for(i in 1:length(week_strata)) {
    dam_week[which(proc_trap_date$TrapDate %within% week_strata[[i]])] = i
  }

  tv_list = list(n.weeks = length(week_strata),
                 dam_week = dam_week)

  return(tv_list)

}
