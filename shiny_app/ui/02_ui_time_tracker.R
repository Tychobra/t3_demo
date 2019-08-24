tabItem(
  tabName = "time_tracker",
  fluidRow(
    box(
      width = 12,
      fluidRow(
        column(
          12,
          div(
            style = "display: inline;",
            p("Rows: ", style = "display: inline-block;"),
            a(
              class = "num_rows bold",
              id = "10",
              style = "display: inline-block;",
              href = "#",
              target = NA,
              "10"
            ),
            p(" | ", style = "display: inline-block;"),
            a(
              class = "num_rows",
              id = "25",
              style = "display: inline-block;",
              href = "#",
              target = NA,
              "25"
            ),
            p(" | ", style = "display: inline-block;"),
            a(
              class = "num_rows",
              id = "50",
              style = "display: inline-block;",
              href = "#",
              target = NA,
              "50"
            ),
            p(" | ", style = "display: inline-block;"),
            a(
              class = "num_rows",
              id = "100",
              style = "display: inline-block;",
              href = "#",
              target = NA,
              "100"
            ),
            p(" | ", style = "display: inline-block;"),
            a(
              class = "num_rows",
              id = "All",
              style = "display: inline-block;",
              href = "#",
              target = NA,
              "All"
            ),
            div(
              style = "display: inline-block; margin-left: 10px; margin-bottom: 5px;",
              # actionButton(
              #   "time_tracker_calc",
              #   "Calculate Hours",
              #   icon = icon("calculator")
              # ) %>% disabled(),
              actionButton(
                "time_tracker_commit",
                "Save Changes",
                icon = icon("check"),
                class = "btn-success color_white"
              ) %>% disabled(),
              actionButton(
                "time_tracker_discard",
                "Discard Changes",
                icon = icon("times"),
                class = "btn-danger color_white"
              ) %>% disabled()
            ),
            div(
              class = "pull-right",
              style = "display: inline-block",
              downloadButton(
                "download_hours",
                "Download"
              )
            )
          )
        ),
        column(
          12,
          fluidRow(
            column(
              12,
              rHandsontableOutput("handson_out")
            )
          ),
          fluidRow(
            column(
              12,
              div(
                div(
                  style = "display: inline-block; width: 200px; margin-left: 250px;",
                  DTOutput("hours_summary")
                ),
                div(
                  style = "display: inline-block; width: 150px; margin-left: 100px;",
                  DTOutput("project_codes")
                )
              )
            )
          )
        )
      )
    )
  )
)
