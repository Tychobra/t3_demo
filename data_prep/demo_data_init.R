# script to take our actual hours, scrub them, and move them to an SQLite database
# for the demo
library(dplyr)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "config.yml")

conn <- tychobratools::db_connect(config$db)

hours <- conn %>%
  tbl("hours2") %>%
  collect()

DBI::dbDisconnect(conn)


client_projects <- tribble(
  ~client,      ~project,
  "Client A",   "Machine Learning",
  "Client A",   "AI Dashboard",
  "Client B",   "Predictive Modeling",
  "Client B",   "Monte Carlo",
  "Cleitn C",   "MCMC",
  "Client C",   "Simulation Machine",
  "Client C",   "Market Analysis",
  "Client D",   "Tensorflow",
  "Client E",   "Neural Net",
  "",           "Admin",
  "",           "Marketing",
  "",           "Research"
)

n_rows <- nrow(hours)

descriptions <- c(
  "Bug fixes",
  "Updated tables",
  "Added charts",
  "Prepped Data",
  "Trained Machine Learning Models",
  "Deep Learning with Tensorflow",
  "created general AI and destroyed humanity",
  "Bitcoin Speculation"
)

hours <- hours %>%
  mutate(
    client_short_name = sample(client_projects$client, n_rows, replace = TRUE),
    description = sample(descriptions, n_rows, replace = TRUE),
    date = as.character(date)
  ) %>%
  select(-id)


# set projects specific to a client

#' sample_project("Client A")
sample_project <- function(client_) {
  projs <- client_projects %>%
    filter(.data$client == client_) %>%
    pull("project")

  sample(projs, 1)
}

for (i in seq_len(n_rows)) {
  hours$project_name[i] <- sample_project(hours$client_short_name[i])
}

conn2 <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = "shiny_app/data/hours_db.sqlite3"
)

create_hours_query <- "CREATE TABLE hours2 (
  id                      INTEGER PRIMARY KEY AUTOINCREMENT,
  date                    DATE,
  start_time              CHARACTER VARYING (5),
  end_time                CHARACTER VARYING (5),
  time                    REAL,
  client_short_name       TEXT,
  project_name            TEXT,
  description             TEXT,
  billable_hourly         BOOLEAN,
  time_created            TIMESTAMP WITH TIME ZONE,
  created_by              TEXT,
  time_modified           TIMESTAMP WITH TIME ZONE,
  modified_by             TEXT
)"

DBI::dbExecute(conn2, "DROP TABLE IF EXISTS hours2")
DBI::dbExecute(conn2, create_hours_query)

DBI::dbWriteTable(
  conn2,
  "hours2",
  hours,
  append = TRUE
)

DBI::dbListTables(conn2)

DBI::dbDisconnect(conn2)
