
calendar_data_prep <- reactive({
  hours_commit_trigger()

  hold <- dash_hours_filter()

  dates <- tibble(date = seq(input$dash_date_range[1], input$dash_date_range[2], by = "days"))
  
  start_date_offset <- as.POSIXlt(input$dash_date_range[1])$wday
  end_date_offset <- 6 - as.POSIXlt(input$dash_date_range[2])$wday

  if (start_date_offset == 0 && end_date_offset == 0) {
    extra_dates <- character(0)
  } else if (start_date_offset == 0) {
    extra_dates <- seq(input$dash_date_range[2] + 1, input$dash_date_range[2] + end_date_offset, by = "days")
  } else if (end_date_offset == 0) {
    extra_dates <- seq(input$dash_date_range[1] - start_date_offset, input$dash_date_range[1] - 1, by = "days")
  } else {
    extra_dates <- c(
      seq(input$dash_date_range[1] - start_date_offset, input$dash_date_range[1] - 1, by = "days"),
      seq(input$dash_date_range[2] + 1, input$dash_date_range[2] + end_date_offset, by = "days")
    )
  }
  
  extra_dates <- tibble(
    date = extra_dates,
    out_of_range = rep(TRUE, length(extra_dates))
  )
  
  hold <- hold %>%
    right_join(dates, by = "date")
  if (nrow(extra_dates) > 0) {
    hold <- full_join(hold, extra_dates, by = "date")
  } else {
    hold <- mutate(hold, out_of_range = NA)
  }
  hold <- hold %>% 
    mutate(time = ifelse(is.na(time), 0, time)) %>%
    mutate(time = ifelse(is.na(out_of_range), time, -1)) %>%
    group_by(date) %>%
    summarise(time = sum(time)) %>%
    ungroup %>%
    mutate(
      week_day = as.POSIXlt(date)$wday,
      month = month(date, label = TRUE)
    )

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
          y = out$time[x]
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
      ),
      height = "100%"
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
          return '<div class=\"arrow_box\" style=\"padding: 10px\">' +
            '<span>' + series[seriesIndex][dataPointIndex].toFixed(2) + '</span>' +
            '</div>'
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
    ax_series2(calendar_data_prep()) %>%
    ax_chart(parentHeightOffset = 0)
})

#output$cal_chart <- renderUI({
#  apexchartOutput("calendar_heat_map", height = "150px")
#})