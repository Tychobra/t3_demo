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
    description = sample(c("Bug fixes", "Updated tables", "Added charts", "Prepped Data"), nrow(hours), replace = TRUE)
  )


conn2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

DBI::dbWriteTable(
  conn2,
  "hours",
  hours
)

DBI::dbListTables(conn2)

DBI::dbDisconnect(conn2)
