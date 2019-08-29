function(input, output, session) {

  # user for demo
  user <- reactive({
    "tycho.brahe@tychobra.com"
  })

  output$authed_user <- renderText({user()})

  source("server/01.1_s_dashboard.R", local = TRUE)
  source("server/02_s_time_tracker.R", local = TRUE)

}


