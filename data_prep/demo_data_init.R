library(dplyr)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "config.yml")

conn <- tychobratools::db_connect(config$db)

hours <- conn %>% 
  tbl("hours2") %>% 
  collect()

DBI::dbDisconnect(conn)

hours <- hours %>% 
  mutate(
    client_short_name = sample(c("client_a", "client_b", "client_c", ""), nrow(hours), replace = TRUE),
    project_name = ifelse(
      client_short_name == "", 
      sample(c("admin", "research", "marketing"), nrow(hours), replace = TRUE),
      paste0(client_short_name, "_", sample(c("proj_1", "proj_2", "proj_3"), nrow(hours), replace = TRUE))
    ),
    description = sample(c("Bug fixes", "Updated tables", "Added charts", "Prepped Data"), nrow(hours), replace = TRUE),
    date = as.character(date)
  ) %>% 
  select(-id)

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
