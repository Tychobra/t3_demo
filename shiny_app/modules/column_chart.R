
#' column_chart_ui
#'
column_chart_ui <- function(id) {
  ns <- NS(id)

  highchartOutput(
    ns("dash_hours_chart"),
    height = 500
  ) %>% withSpinner()
}


#' column_chart
#'
#' @param hours_gouped reactive - a data frame of hours summarized by the user selected `groups`.  The
#' data frame has either 2, 3, or 4 columns (depending on the number of `groups` selected).
#'  - year_month  - character - the year and month in "yyyy-mm" format
#'  - client_short_name - character - the short name for the client.  This column only exists if "client_short_name"
#'  is in `groups`.
#'  - project_name - character - the project name.  This column only exists if "project_name" is in `groups`.
#'  - time - numeric - the total time in grouped by the columns to the left
#'  @param groups reactive - the user selected groups. Possible values are NULL, and 1 or both of "client_short_name"
#'  and "project_name"
#'  @param date_range reactive - Date vector of length 2
#'
column_chart <- function(input, output, session, hours_grouped, groups, date_range) {

  dash_hours_chart_prep <- reactive({
    hold <- hours_grouped()
    groups <- groups()

    hold <- hold %>%
      mutate(
        date = paste0(year_month, "-01") %>% as.Date()
      )



    if (length(groups) == 2) {
      hold$series <- paste0(hold[[groups[1]]], "::", hold[[groups[2]]])
    } else if (length(groups) == 1) {
      hold$series <- hold[[groups]]
    } else if (is.null(groups)){
      # just a placeholder string so that data frame always has the series column
      hold$series <- "no_goups"
    }

    if (length(unique(hold$series)) > 15) {
      series_names <- (hold %>%
                         group_by(series) %>%
                         summarise(total_time = sum(time)) %>%
                         ungroup %>%
                         arrange(desc(total_time)) %>%
                         pull(series))[1:14]

      other <- hold %>%
        filter(!(series %in% series_names)) %>%
        group_by(date) %>%
        summarise(time = sum(time)) %>%
        ungroup %>%
        mutate(series = "other")

      hold <- hold %>%
        filter(series %in% series_names) %>%
        select(series, time, date) %>%
        rbind(other)
    }

    uniq_series <- unique(hold$series)

    out <- lapply(uniq_series, function(series) {
      hold_series <- hold[hold$series == series, ]

      list(
        data = as.xts(hold_series$time, order.by = hold_series$date),
        name = if (series == "<NA>") "NA" else series
      )

    })

    out
  })



  output$dash_hours_chart <- renderHighchart({
    plot_data <- dash_hours_chart_prep()
    groups <- groups()
    date_range <- date_range()

    legend_title <- ""
    if (length(groups) == 1) {
      legend_title <- names(group_choices)[group_choices == groups]
    } else if (length(groups) == 2) {
      legend_title <- paste(names(group_choices), collapse = "::")
    }


    hc_out <- highchart() %>%
      hc_chart(
        type = "column"
      ) %>%
      hc_colors(
        more_colors
      ) %>%
      hc_title(
        text = "Hours Per Month"
      ) %>%
      hc_subtitle(
        text = paste0(date_range[1], " to ", date_range[2])
      ) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = tychobratools::hc_btn_options()
      ) %>%
      hc_xAxis(
        type = "datetime"
      ) %>%
      hc_tooltip(
        pointFormat = "{series.name}: <b>{point.y:,.0f}</b>"
      ) %>%
      hc_yAxis(
        title = list(text = "Hours"),
        stackLabels = list(
          enabled = TRUE,
          style = list(
            fontWeight = "bold",
            color = "#f7a35c",
            textOutline = NULL
          ),
          format = "{total:,.2f}"
        )
      ) %>%
      hc_plotOptions(
        column = list(
          stacking = "normal"
        )
      ) %>%
      hc_legend(
        reversed = TRUE,
        enabled = if (is.null(groups)) FALSE else TRUE,
        title = list(
          text = legend_title
        )
      )

    for (i in seq_along(plot_data)) {
      hc_out <- hc_out %>%
        hc_add_series(
          data = plot_data[[i]]$data,
          name = plot_data[[i]]$name
        )
    }

    hc_out
  })


}