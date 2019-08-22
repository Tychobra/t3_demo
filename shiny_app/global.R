library(shiny)
library(shinydashboard)
library(tychobratools) # remotes::install_github("tychobra/tychobratools")
library(lubridate)
library(dplyr)
library(RPostgres)
library(DBI)
library(pool)
library(shinyWidgets)
library(config)
library(rhandsontable)
library(highcharter)
library(xts)
library(tidyr)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(polished)
library(gh)
library(apexcharter)
library(shinytoastr)

tychobratools::hc_global_options()

# turn off scientific notation
options(scipen = 999)
options(spinner.type = 8)

# TODO: add to developers table in database
github_usernames <- tribble(
  ~username,                       ~email,
  "merlinoa", "andy.merlino@tychobra.com",
  "phoward38", "phoward38@gatech.edu",
  "RichardHHill", "richard_hill@brown.edu",
  "jimbrig2011", "jimmy.briggs@oliverwyman.com"
)

#polished::set_config_env()
Sys.setenv(R_CONFIG_ACTIVE = "default")
#Sys.setenv(R_CONFIG_ACTIVE = "production")
#Sys.setenv(R_CONFIG_ACTIVE = "sqlite_test")
#api <- source("data/api.R", local = TRUE)$value()

source("helpers/time_hours.R", local = TRUE)


# # get db config file
# app_config <- NULL
# try(
#   app_config <- config::get(file = "config.yml", config = "default"),
#   silent = TRUE
# )
# 
# # set up pool connection
# if (is.null(app_config)) {
#   stop("Missing or invalid config file")
# } else {

pool <- pool::dbPool(
  drv = RSQLite::SQLite(),
  dbname = "data/hours_db.sqlite3"
)

# }

# disconnect pool connection when app exits
onStop(function() {
  pool::poolClose(pool)
})


# choices for grouping in dashboard
group_choices <- c(
  "Client" = "client_short_name",
  "Project" = "project_name"
)

start_date <- as.Date("2018-01-02")




valueBox2 <- function (value, subtitle, icon = NULL, backgroundColor = "#7cb5ec", color = "#FFF", width = 4, href = NULL) {

  boxContent <- div(
    class = "small-box",
    style = paste0("background-color: ", backgroundColor, "; color: ", color),
    div(
      class = "inner",
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) {
      div(class = "icon-large", icon)
    }
  )

  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

# options for highcharter download button
hc_btn_options <- function() {
  list(
    contextButton = list(
      enabled = FALSE
    ),
    exportButton = list(
      text = "Export",
      symbol = "download",
      menuItems = list(
        list(
          text = "Export to PDF",
          onclick = JS(
            "function () {
              this.exportChart({
                type: 'application/pdf'
              });
            }"
          )
        ),
        list(
          text = "Export to PNG",
          onclick = JS(
            "function () {
              this.exportChart(null, {
                chart: {
                  backgroundColor: '#FFFFFF'
                },
              });
            }"
          )
        )
      )
    )
  )
}


more_colors <- c(
  # highcharts colors
  "#7cb5ec",
  "#434348",
  "#90ed7d",
  "#f7a35c",
  "#8085e9",
  "#f15c80",
  "#e4d354",
  "#2b908f",
  "#f45b5b",
  "#91e8e1",

  # from color brewer
  "#ffffd9",
  "#7fcdbb",
  "#41b6c4",
  "#1d91c0",
  "#edf8b1",
  "#225ea8",
  "#253494",
  "#c7e9b4",
  "#081d58"
)