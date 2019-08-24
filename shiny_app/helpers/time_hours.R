#### some functions for calculating hours based on time
valid_times <- (function() {
  hours <- 0:23
  hours <- as.character(hours)
  #hours <- ifelse(nchar(hours) == 1, paste0("0", hours), hours)
  hours <- rep(hours, each = 4)
  minutes <- c(":00", ":15", ":30", ":45")

  paste0(hours, minutes)
})()



#' check if time is valid
#'
#'
#' @param my_time times to check
#'
#' @examples
#'
#' test_time <- "8:30"
#'
#' test_times <- c("7:15", "12:30")
#'
#' is_time_valid(test_time)
#' is_time_valid(test_times)
#'
#'
is_time_valid <- function(my_time) {
  my_time %in% valid_times
}
is_time_valid_vec <- Vectorize(is_time_valid, USE.NAMES = FALSE)

#' diff_time
#'
#' calculate the difference in hours between two character strings of form
#' "h:mm" or "hh:mm"
#'
#' @param time_start character string of form "h:mm" or "hh:mm"
#' @param time_end character string of form "h:mm" or "hh:mm"
#'
#' @return time in hours of `end_time` - `start_time`
#'
#' @examples
#' time_start <- "8:15"
#' time_end <- "10:30"
#'
#' diff_time(time_start, time_end)
#'
diff_time <- function(time_start, time_end) {
  time_end <- time_to_hours_vec(time_end)
  time_start <- time_to_hours_vec(time_start)

  time_end <- ifelse(time_start > time_end, time_end + 24, time_end)

  time_end - time_start
}

#' time_to_hours
#'
#' convert a character of form "h:mm" or "hh:mm" to time in hours
#'
#' @param time of form "h:mm" or "hh:mm"
#'
#' @return numeric time in hours
#'
#' @examples
#' my_time <- c("10:30", "11:00")
#'
#' time_to_hours(my_time)
#'
time_to_hours <- function(.time) {
  hour_start <- gsub(":.*", "", .time) %>% as.numeric()
  min_start <- gsub(".*:", "", .time) %>% as.numeric()

  min_start <- min_start / 60

  hour_start + min_start
}

time_to_hours_vec <- function(x) unlist(lapply(x, function(x) time_to_hours(x)))



#' create blank rows
#'
#' these go on the bottom of the time tracker table
#'
#' @param nume_rows the number of rows to create
#'
#' @examples
#'
#' create_blank_rows(10)
#'
create_blank_rows <- function(num_rows) {
  tibble(
    id = rep(NA_integer_, num_rows),
    date = rep(NA_character_, num_rows) %>% as.Date(),
    start_time = rep(NA_character_, num_rows),
    end_time = rep(NA_character_, num_rows),
    time = rep(NA_real_, num_rows),
    client_short_name = rep("", num_rows),
    project_name = rep("", num_rows),
    description = rep("", num_rows)
  )
}
