fluidRow(
  column(
    9,
    fluidRow(
      box(
        width = 12,
        highchartOutput(
          "dash_hours_chart",
          height = 500
        ) %>% withSpinner()
      ),
      box(
        width = 12,
        apexchartOutput("calendar_heat_map", height = "150px")
      ),
      box(
        width = 12,
        DTOutput("dash_hours_table") %>% withSpinner()
      )
    )
  ),
  column(
    3,
    fluidRow(
      valueBoxOutput(
        "dash_time_billable_box_out",
        width = 12
      ),
      box(
        width = 12,
        title = "Groups",
        div(
          class = "text-center",
          checkboxGroupInput(
            "dash_groups",
            NULL,
            choices = group_choices,
            selected = group_choices[1],
            inline = TRUE
          )
        )
      ),
      box(
        width = 12,
        title = "Filters",
        dateRangeInput(
          "dash_date_range",
          "Date Range",
          start = max(Sys.Date() - years(1), start_date),
          min = start_date,
          end = Sys.Date(),
          max = Sys.Date(),
          startview = "year"
        ),
        br(),
        shinyWidgets::pickerInput(
          inputId = "dash_clients",
          label = "Clients",
          choices = NULL,
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = NULL
        ),
        br(),
        shinyWidgets::pickerInput(
          inputId = "dash_projects",
          label = "Projects",
          choices = NULL,
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = NULL
        )
      )
    )
  )
)
