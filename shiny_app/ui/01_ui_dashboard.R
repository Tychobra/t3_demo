tabItem(
  tabName = "dashboard",
  fluidRow(
    column(
      9,
      fluidRow(
        box(
          width = 12,
          column_chart_ui("column_chart")
        ),
        box(
          width = 12,
          calendar_chart_ui("cal")
        )
      )
    ),
    column(
      3,
      fluidRow(
        tychobratools::value_box_module_ui(
          "dash_time_billable_box_out",
          icon = icon("clock-o"),
          width = 12,
          backgroundColor = hc_default_colors()[1]
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
  ),
  fluidRow(
    box(
      width = 12,
      dash_table_ui("dash_table")
    )
  )
)
