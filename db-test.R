library(RMySQL)
library(openssl)
con <- dbConnect(RMySQL::MySQL(), host = "sql189.main-hosting.eu", dbname = "u308762622_shiny",  user = "u308762622_doab", password = "VT16Drasov")
summary(con)
dbGetQuery(con, paste0("INSERT INTO users (username, passwd, roles, access_date) VALUES ('cerveny.j', '", sha1("Zamek37333", key = "!VP5.1"), "', 99, NOW());"))
dbDisconnect(con)

