dbCon <- dbConnect(RMySQL::MySQL(), host = "sql189.main-hosting.eu", dbname = "u308762622_shiny",  user = "u308762622_doab", password = "VT16Drasov")
dbGetQuery(dbCon, paste0("INSERT INTO users (username, passwd, first_name, last_name, email, organization, department) VALUES (", input.register$regUsername, sha1(input.register$regPassword, key = '!VP5.1'), input.register$regName, input.register$regSurname, input.register$regEmail, input.register$regOrganization, input.register$regDepartment, ");"))
dbDisconnect(dbCon)
