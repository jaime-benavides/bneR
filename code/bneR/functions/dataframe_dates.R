#' create data frame with dates
#'
#' @export data_frame_dates
#'
#########################################################################
data_frame_dates <- function(date_begin = date_begin, date_end = date_end) {
  # set temporal context
  year_begin <- date_begin[1]
  month_begin <- date_begin[2]
  day_begin <- date_begin[3]
  # End
  year_end <- date_end[1]
  month_end <- date_end[2]
  day_end <- date_end[3]
  date_t_begin <- ISOdate(year_begin, month_begin,
                          day_begin, tz = "UTC")
  date_t_end <- ISOdate(year_end, month_end, day_end, tz = "UTC")
  all.dates <- data.frame(
    date = seq(date_t_begin,date_t_end, by = "day"))
  return(all.dates)
}
