



#' calendar_chart_ui
calendar_chart_ui <- function(id) {
  ns <- NS(id)

  apexchartOutput(
    ns("calendar_heat_map"),
    height = "150px"
  ) %>% withSpinner()
}

#' calendar_chart
#'
#' Shiny module server logic for calendar chart
#'
#' @param hours reactive - data frame of user submitted hours - 8 columns
#'  - id the id for the row
#'  - date the date of the time entry
#'  - start_time the start time
#'  - end_time the end time
#'  - time the number of hours between the start and end time
#'  - client_short_name the shortened name for the client
#'  - project_name the name of the project
#'  - description description of work during time
#' @param date_range Date vector of length 2.  The user selected date range.  The `hours`
#' are filtered by this date range
#'
#'
calendar_chart <- function(input, output, session, hours, date_range) {
  # the heatmap always displays a rectangle, but we want the days outside the range (in the columns
  # on the far left and right) to look like they are not part of the heatmap.  In order to achieve this
  # we first need to identify the "extra_days".  These are the days inside the heatmap rectangle, but
  # outside the selected date range
  extra_days <- reactive({
    req(date_range())

    date_range <- date_range()

    # these offsets are used to clear the coloring in the days outside the date range on the far
    # left and right of the calendar chart
    start_date_offset <- as.POSIXlt(date_range[1])$wday
    end_date_offset <- 6 - as.POSIXlt(date_range[2])$wday

    if (start_date_offset == 0 && end_date_offset == 0) {
      extra_dates <- character(0)
    } else if (start_date_offset == 0) {
      extra_dates <- seq(date_range[2] + 1, date_range[2] + end_date_offset, by = "days")
    } else if (end_date_offset == 0) {
      extra_dates <- seq(date_range[1] - start_date_offset, date_range[1] - 1, by = "days")
    } else {
      extra_dates <- c(
        seq(date_range[1] - start_date_offset, date_range[1] - 1, by = "days"),
        seq(date_range[2] + 1, date_range[2] + end_date_offset, by = "days")
      )
    }

    tibble(
      date = extra_dates,
      out_of_range = rep(TRUE, length(extra_dates))
    )
  })

  calendar_days <- reactive({
    req(date_range())
    hold <- hours()
    date_range <- date_range()
    all_dates <- tibble(date = seq(date_range[1], date_range[2], by = "days"))
    extra_dates <- extra_days()

    # make sure to include all dates in date range, even if there are no hours for
    # a date
    hold <- all_dates %>%
      left_join(hold, by = "date")

    if (nrow(extra_dates) > 0) {
      hold <- full_join(hold, extra_dates, by = "date")
    } else {
      hold <- mutate(hold, out_of_range = NA)
    }

    hold %>%
      mutate(
        time = ifelse(is.na(time), 0, time),
        time = ifelse(is.na(out_of_range), time, -1)
      ) %>%
      mutate() %>%
      group_by(date) %>%
      summarise(time = sum(time)) %>%
      ungroup() %>%
      mutate(
        week_day = as.POSIXlt(date)$wday,
        month = month(date, label = TRUE)
      )
  })

  calendar_chart_prep <- reactive({
    hold <- calendar_days()

    weekdays <- c("", "Mon", "", "Wed", "", "Fri", "")

    lapply(7:1, function(x) {
      out <- hold %>%
        filter(week_day == x - 1)

      list(
        name = weekdays[x],
        data = lapply(seq_along(out$time), function(x) {
          list(
            # -1 and 0 would make it the first week of the month. -2 and -1 makes it the second,
            # putting it more in the middle of the month
            x = if(isTRUE(out$month[x - 2] != out$month[x - 1])) out$month[x] else "",
            y = out$time[x],
            date = out$date[x]
          )
        })
      )
    })
  })



  output$calendar_heat_map <- renderApexchart({
    apexchart() %>%
      ax_chart(
        type = "heatmap",
        toolbar = list(
          show = FALSE
        )
      ) %>%
      ax_dataLabels(enabled = FALSE) %>%
      ax_colors("#7cb5ec") %>%
      ax_legend(show = FALSE) %>%
      ax_xaxis(
        tooltip = list(
          enabled = FALSE
        ),
        labels = list(
          rotate = 1,
          trim = FALSE
        ),
        axisTicks = list(
          show = FALSE
        ),
        axisBorder = list(
          show = FALSE
        )
      ) %>%
      ax_tooltip(
        custom = JS("function({series, seriesIndex, dataPointIndex, w}) {
        if (series[seriesIndex][dataPointIndex] < 0 ) {
          return ''
        } else {
          var val = series[seriesIndex][dataPointIndex];
          var color;
          switch(true) {
            case (val < 0.1):
              color = '#e0e0e0'
              break;
            case (val < 3):
              color = '#c2d3ff';
              break;
            case (val < 6):
              color = '#8faeff';
              break;
            case (val < 9):
              color = '#5987ff';
              break;
            case (val < 12):
              color = '#2b66ff';
              break;
            default:
              color = '#0047ff';
              break;
          }


          var date =  w.config.series[seriesIndex].data[dataPointIndex].date
          var month =  date.substring(5, 7)

          if (month.substring(0, 1) === '0') {
            month = month.substring(1, 2)
          }
          var day =  date.substring(8, 10)
          if (day.substring(0, 1) === '0') {
            day = day.substring(1, 2)
          }
          var display_date = month + '/' + day

          // if point is from this year, do not show the year in the tooltip.  If it is from a prior
          // year, then show the year
          var today = new Date()
          var today_year = today.getFullYear()
          today_year = today_year.toString()

          var point_year = date.substring(0, 4)

          if (today_year !== point_year) {
            display_date = display_date + '/' + point_year.substring(2, 4)
          }

          return (
            '<div style=\"padding: 10px;\">' +
              '<div style =\"display: inline-block; width: 10px; height: 10px; margin-right: 5px; background-color:' + color + ';\"></div>' +
              '<span style = \"font-weight:bold;\">' + series[seriesIndex][dataPointIndex].toFixed(2) + ' hrs </span>' +
              '<span style= \"margin-right: 5px;\">on ' + display_date + '</span>' +
            '</div>'
          )
        }
      }")
      ) %>%
      ax_plotOptions(
        heatmap = list(
          enableShades = FALSE,
          colorScale = list(
            ranges = list(
              list(
                from = -1.5,
                to = -0.5,
                color = "#ffffff"
              ),
              list(
                from = 0,
                to = 0.1,
                color = "#e0e0e0"
              ),
              list(
                from = 0.1,
                to = 3,
                color = "#c2d3ff"
              ),
              list(
                from = 3,
                to = 6,
                color = "#8faeff"
              ),
              list(
                from = 6,
                to = 9,
                color = "#5987ff"
              ),
              list(
                from = 9,
                to = 12,
                color = "#2b66ff"
              ),
              list(
                from = 12,
                to = 24,
                color = "#0047ff"
              )
            )
          )
        )
      ) %>%
      ax_series2(calendar_chart_prep())
  })
}


