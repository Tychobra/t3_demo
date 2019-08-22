
dash_hours_prep <- reactive({
  req(hours())
  out <- hours()
  
  out %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date),
      month = ifelse(nchar(month) == 1, paste0("0", month), month),
      year_month = paste0(year, "-", month),
      client_short_name = ifelse(client_short_name == "", "<NA>", client_short_name),
      project_name = ifelse(project_name == "", "<NA>", project_name)
    )
})


dash_hours_filter <- reactive({
  req(input$dash_clients, input$dash_projects)
  out <- dash_hours_prep()

  out <- out %>%
    filter(
      date >= input$dash_date_range[1],
      date <= input$dash_date_range[2],
      client_short_name %in% input$dash_clients,
      project_name %in% input$dash_projects
    )

  req(nrow(out) > 0)

  out
})



dash_hours_group <- reactive({
  out <- dash_hours_filter()
  groups <- input$dash_groups

  out <- out %>%
    group_by(year_month)

  if (!is.null(groups)) {
    out <- out %>%
      group_by(!!!syms(groups), add = TRUE)
  }

  out <- out %>%
    summarize(time = sum(time)) %>%
    ungroup() %>%
    arrange(year_month)

  out
})

clients <- reactive({
  req(dash_hours_prep())

  dash_hours_prep() %>%
    pull("client_short_name") %>%
    unique() %>%
    sort()
})



observeEvent(clients(), {
  updatePickerInput(
    session,
    "dash_clients",
    choices = clients(),
    selected = clients()
  )
})

project_names <- reactive({
  req(dash_hours_prep())

  out <- dash_hours_prep() %>%
    pull("project_name") %>%
    unique() %>%
    sort()
})

observeEvent(project_names(), {
  updatePickerInput(
    session,
    "dash_projects",
    choices = project_names(),
    selected = project_names()
  )
})

dash_time_total <- reactive({
  dash_hours_group() %>%
    pull(time) %>%
    sum(na.rm = TRUE)
})

output$dash_time_billable_box_out <- renderValueBox({
  valueBox2(
    value = dash_time_total() %>% round(2) %>% format(big.mark = ","),
    subtitle = "Total Hours",
    icon = icon("clock-o"),
    backgroundColor = hc_default_colors()[1]
  )
})



dash_hours_chart_prep <- reactive({
  hold <- dash_hours_group()

  hold <- hold %>%
    mutate(
      date = paste0(year_month, "-01") %>% as.Date()
    )

  groups <- input$dash_groups

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
  groups <- input$dash_groups
  date_range <- input$dash_date_range

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
      buttons = hc_btn_options()
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




dash_table_prep <- reactive({
  out <- dash_hours_group()
  groups <- input$dash_groups


  out <- out %>%
    spread(key = year_month, value = time)


  n_cols <- length(out)

  n_group_vars <- length(input$dash_groups)

  start_time_cols <- 1 + n_group_vars

  out$Total <- rowSums(out[, start_time_cols:n_cols], na.rm = TRUE)

  out %>%
    arrange(desc(Total))
})


# create the column headers for the table
hours_table_headers <- reactive({
  groups <- input$dash_groups
  tbl_dat <- dash_table_prep()
  tbl_len <- length(dash_table_prep())

  if (is.null(groups)) {
    totals <- lapply(tbl_dat, function(x) {
      sum(x, na.rm = TRUE)
    })

    out <- htmltools::withTags(table(
      thead(
        tr(
          th(colspan = tbl_len, "Hours Per Month")
        ),
        tr(
          lapply(names(tbl_dat), th)
        )
      ),
      tableFooter(format(totals, big.mark = ","))
    ))
  } else if (length(groups) == 1) {
    totals <- lapply(tbl_dat[, -1], function(x) {
      sum(x, na.rm = TRUE)
    })

    out <- htmltools::withTags(table(
      thead(
        tr(
          th(rowspan = 2, names(group_choices)[group_choices == names(tbl_dat)[1]]),
          th(colspan = tbl_len - 1, "Hours Per Month")
        ),
        tr(
          lapply(names(tbl_dat)[-1], th)
        )
      ),
      tableFooter(c("Total: ", format(totals, big.mark = ",")))
    ))
  } else if (length(groups) == 2) {
    totals <- lapply(tbl_dat[, -c(1:2)], function(x) {
      sum(x, na.rm = TRUE)
    })

    out <- htmltools::withTags(
      table(
        thead(
          tr(
            th(rowspan = 2, names(group_choices)[group_choices == names(tbl_dat)[1]]),
            th(rowspan = 2, names(group_choices)[group_choices == names(tbl_dat)[2]]),
            th(colspan = tbl_len - 2, "Hours Per Month")
          ),
          tr(
            lapply(names(tbl_dat)[-c(1:2)], th)
          )
        ),
        tableFooter(c("Total: ", " ", format(totals, big.mark = ",")))
      )
    )
  }



})

output$dash_hours_table <- renderDT({
  out <- dash_table_prep()

  datatable(
    out,
    class = "cell-border stripe compact",
    container = hours_table_headers(),
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      dom = "Bt",
      pageLength = nrow(out),
      buttons = list(
        list(
          extend = "excel",
          text = "Download",
          title = paste0("hours-monthly-", Sys.Date())
        )
      ),
      scrollX = TRUE
    )
  ) %>%
    formatStyle(
      1,
      target = 'row',
      backgroundColor = styleEqual("Total", "#ccc")
    ) %>%
    formatStyle(
      "Total",
      backgroundColor = "#ccc"
    )
})