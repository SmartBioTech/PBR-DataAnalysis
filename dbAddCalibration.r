dbCon <- dbConnect(RMySQL::MySQL(), host = "sql189.main-hosting.eu", dbname = "u308762622_shiny",  user = "u308762622_doab", password = "VT16Drasov")
dbGetQuery(dbCon, paste0("INSERT INTO calibrations (created_date, description, instrument_id, measured_date, user_id, slope, intercept, temp_slope, temp_intercept) VALUES (NOW());"))
dbDisconnect(dbCon)
