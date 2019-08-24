
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

  out %>%
    summarize(time = sum(time)) %>%
    ungroup() %>%
    arrange(year_month)
})



clients <- reactive({
  req(hours())

  hours() %>%
    pull("client_short_name") %>%
    unique() %>%
    sort()
})



client_choices <- reactive({
  ifelse(clients() == "", "<NA>", clients())
})

observeEvent(clients(), {
  updatePickerInput(
    session,
    "dash_clients",
    choices = client_choices(),
    selected = client_choices()
  )
})

projects <- reactive({
  req(hours())

  out <- hours() %>%
    pull("project_name") %>%
    unique() %>%
    sort()
})


observeEvent(projects(), {
  updatePickerInput(
    session,
    "dash_projects",
    choices = projects(),
    selected = projects()
  )
})

dash_time_total <- reactive({
  dash_hours_group() %>%
    pull(time) %>%
    sum(na.rm = TRUE)
})


callModule(
  value_box_module,
  id = "dash_time_billable_box_out",
  value = reactive({dash_time_total() %>% round(2) %>% format(big.mark = ",")}),
  subtitle = function() "Total Hours"
)

callModule(
  column_chart,
  id = "column_chart",
  hours_grouped = dash_hours_group,
  groups = reactive({input$dash_groups}),
  date_range = reactive({input$dash_date_range})
)

callModule(
  calendar_chart,
  "cal",
  hours = dash_hours_filter,
  date_range = reactive({input$dash_date_range})
)

callModule(
  dash_table,
  id = "dash_table",
  hours_grouped = dash_hours_group,
  groups = reactive({input$dash_groups}),
  date_range = reactive({input$dash_date_range})
)
