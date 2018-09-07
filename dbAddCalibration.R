dbCon <- dbConnect(RMySQL::MySQL(), dbname = 'ShinyApps', username = 'ShinyApps', password = 'GCRI-DoAB', host = 'gcri-doab-ext.diskstation.me', port = 33307)
dbWriteTable(dbCon, 'PBRDataAnalysis_Calibrations', data.frame(date = "2018-01-08", PBR = 72700006, tyoe = "dO2", m = 20.3, b = 557.4), row.names = FALSE, append = TRUE)
dbDisconnect(dbCon)
