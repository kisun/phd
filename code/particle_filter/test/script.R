library(RPostgreSQL)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")

shape = dbGetQuery(con, "SELECT * FROM shapes WHERE shape_id='514' ORDER BY pt_sequence")
