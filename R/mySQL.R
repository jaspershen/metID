# library("RMySQL")
# pryr::mem_used()
# shendatabase <- dbConnect(MySQL(), 
#                   dbname = "shendatabase", 
#                   username = "root", 
#                   password = "shen1990",
#                   host = "127.0.0.1", 
#                   port = 3306)
# pryr::mem_used()
# # Create a connection Object to MySQL database.
# # We will connect to the sampel database named "testdb" that comes with MySql installation.
# 
# dbListTables(shendatabase)
# dbGetInfo(dbObj = shendatabase)
# summary(shendatabase, verbose = TRUE)
# 
# 
# 
# fruits <-data.frame(id=1:5,name=c("apple","banana","pear","corn","melon"),
#                     price=c(8.8,4.98,7.8,6,2.1),
#                     status=c("non","discount","NA","saleout","wholesale"))  
# dbWriteTable(conn = shendatabase, name = "fruits", value = fruits, overwrite = TRUE) 
# # dbSendQuery(conn = shendatabase, statement = 'drop table if exists t_demo')
# dbListTables(shendatabase)
# dbReadTable(conn = shendatabase, name = "fruits")
# dbGetQuery(conn = shendatabase, "SELECT * FROM fruits limit 5")  
# res <- dbSendQuery(conn = shendatabase, statement = "SELECT *FROM fruits")  
# data <- dbFetch(res = res, n = -1) 
# dbClearResult(res)  
# dbDisconnect(conn = shendatabase)
# 
# colnames(hmdb.info) <- c("HMDB.ID", "CAS.ID", "Compound.name")
# hmdb_info <- hmdb.info
# hmdb_info$Compound.name <- stringr::str_trim(string = hmdb_info$Compound.name, side = "both")
# data.table::
# hmdb_info$Compound.name <- stringr::str_replace_all(string = hmdb_info$Compound.name, pattern = "\\(", replacement = "-")
# RMySQL::dbWriteTable(conn = shendatabase, 
#                      name = "hmdb_info",
#                      value = hmdb_info, 
#                      overwrite = TRUE, 
#                      temporary = TRUE, 
#                      fileEncoding="ascii"
#                      )
# 
# dbWriteTable(conn = shendatabase, name = 'testutf8', value = df, 
#              temporary = TRUE, overwrite = TRUE)
# temp <- dbReadTable(conn = shendatabase, 'testutf8')
# 
# 
# dbListTables(shendatabase)
# dbDisconnect(conn = shendatabase)
# # RMySQL::dbReadTable(conn = conn, name = "t_demo")
# # RMySQL::dbWriteTable(conn = conn, name = "t_demo", value = t_demo, append = TRUE)
# rs <- dbSendQuery(conn = shendatabase, 
#                   statement = "select * from hmdb_info where HMDB.ID = HMDB000001")
# # dbSendQuery(conn = conn, statement = 'drop table if exists t_demo')
# # dbRemoveTable(conn = conn, name = "t_demo", overwrite = TRUE)
# dbListTables(shendatabase)
