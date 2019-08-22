server <- function(input, output, session) {

  callModule(
    profile_module,
    "profile"
  )

  user <- reactive({
    "andy.merlino@tychobra.com"
  })

  source("server/01_s_dashboard.R", local = TRUE)
  source("server/01.2_s_calendar.R", local = TRUE)
  source("server/02_s_time_tracker.R", local = TRUE)
}
