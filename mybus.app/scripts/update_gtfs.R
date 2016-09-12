## update the GTFS stuff
library(RPostgreSQL)
library(jsonlite)
library(caroline)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost", user = "homestead", port = "54320", password = "secret")

system('curl -s -X GET "https://api.at.govt.nz/v2/gtfs/versions" -H "Ocp-Apim-Subscription-Key: 1da1fb03bbed453ca7e429643e6abad1" -o gtfs_versions.json')
versions = fromJSON('gtfs_versions.json')$response
unlink('gtfs_versions.json')

curVer = dbGetQuery(con, "SELECT version FROM gtfs_versions")
new = which(! versions$version %in% curVer$version)
if (length(new) > 0) {
  dbWriteTable(con, "gtfs_versions", versions[new, ], append = TRUE, row.names = FALSE)


  ## Now just go through each file and update things:

  ## --------------------------------------------------------------- AGENCIES
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/agency.txt" -o agencies.csv')
  agencies = read.csv("agencies.csv")
  unlink("agencies.csv")
  curAg = dbGetQuery(con, "SELECT agency_id FROM agencies")
  new = which(! agencies$agency_id %in% curAg$agency_id)
  if (length(new) > 0) {
    agencies = agencies[new, ]
    a = data.frame(
      agency_id = agencies$agency_id,
      name = agencies$agency_name,
      url = agencies$agency_url,
      timezone = agencies$agency_timezone,
      lang = agencies$agency_lang,
      phone = agencies$agency_phone
    )

    dbGetQuery(con,
      sprintf("INSERT INTO agencies (agency_id, name, url, timezone, lang, phone) VALUES ('%s')",
              paste(a$agency_id, a$name, a$url, a$timezone, a$lang, a$phone, sep = "','", collapse = "'), ('")))
  }

  ## --------------------------------------------------------------- ROUTES
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/routes.txt" -o routes.csv')
  routes = read.csv("routes.csv")
  unlink("routes.csv")
  curAg = dbGetQuery(con, "SELECT id FROM routes")
  new = which((! routes$route_id %in% curAg$id) &
              grepl(paste0("-", versions$version, collapse="|"), routes$route_id))
  if (length(new) > 0) {
    routes = routes[new, ]
    a = list(
      id = routes$route_id,
      route_id = gsub(paste0("-", versions$version, collapse="|"), "", routes$route_id),
      short_name = routes$route_short_name,
      long_name = routes$route_long_name,
      type_id = routes$route_type
    )
    a$version = sapply(1:nrow(routes), function(i) gsub(paste0(a$route_id[i], "-"), "", a$id[i]))
    if (!is.null(routes$agency_id)) a$agency_id =  routes$agency_id
    if (!is.null(routes$route_desc)) a$desc = routes$route_desc
    if (!is.null(routes$route_url)) a$url = routes$route_url
    if (!is.null(routes$route_color)) a$color = routes$route_color
    if (!is.null(routes$route_text_color)) a$text_color = routes$route_text_color

    dbGetQuery(con,
      sprintf("INSERT INTO routes (%s) VALUES ('%s')",
              paste(names(a), collapse = ","),
              do.call(paste, c(a, list(sep = "','", collapse = "'),('")))))
  }

  ## --------------------------------------------------------------- CALENDARS
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/calendar.txt" -o calendar.csv')
  cal = read.csv("calendar.csv")
  unlink("calendar.csv")
  curAg = dbGetQuery(con, "SELECT id FROM calendars")
  new = which((! cal$service_id %in% curAg$id) &
              grepl(paste0("-", versions$version, collapse="|"), cal$service_id))
  if (length(new) > 0) {
    cal = cal[new, ]
    a = list(
      id = cal$service_id,
      service_id = gsub(paste0("-", versions$version, collapse="|"), "", cal$service_id),
      monday = cal$monday == 1,
      tuesday = cal$tuesday == 1,
      wednesday = cal$wednesday == 1,
      thursday = cal$thursday == 1,
      friday = cal$friday == 1,
      saturday = cal$saturday == 1,
      sunday = cal$sunday == 1,
      start_date = cal$start_date,
      end_date = cal$end_date
    )
    a$version = sapply(1:nrow(cal), function(i) gsub(paste0(a$service_id[i], "-"), "", a$id[i]))

    dbGetQuery(con,
      sprintf("INSERT INTO calendars (%s) VALUES ('%s')",
              paste(names(a), collapse = ","),
              do.call(paste, c(a, list(sep = "','", collapse = "'),('")))))
  }

  ## --------------------------------------------------------------- CALENDAR DATES
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/calendar_dates.txt" -o calendar_dates.csv')
  cal = read.csv("calendar_dates.csv")
  unlink("calendar_dates.csv")
  curAg = dbGetQuery(con, "SELECT service_id, date FROM calendar_dates")
  new = which((! paste(cal$service_id, cal$date) %in% paste(curAg$service_id, curAg$date)) &
              grepl(paste0("-", versions$version, collapse="|"), cal$service_id))
  if (length(new) > 0) {
    cal = cal[new, ]
    a = list(
      service_id = cal$service_id,
      date = cal$date,
      exception_type = cal$exception_type
    )

    dbGetQuery(con,
      sprintf("INSERT INTO calendar_dates (%s) VALUES ('%s')",
              paste(names(a), collapse = ","),
              do.call(paste, c(a, list(sep = "','", collapse = "'),('")))))
  }


  ## --------------------------------------------------------------- TRIPS
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/trips.txt" -o trips.csv')
  trips = read.csv("trips.csv")
  unlink("trips.csv")
  curAg = dbGetQuery(con, "SELECT id FROM trips")
  new = which((! trips$trip_id %in% curAg$id) &
              grepl(paste0("-", versions$version, collapse="|"), trips$trip_id))
  if (length(new) > 0) {
    trips = trips[new, ]
    a = list(
      id = trips$trip_id,
      route_id = trips$route_id,
      service_id = trips$service_id,
      trip_id = gsub(paste0("-", versions$version, collapse="|"), "", trips$trip_id)
    )
    a$version = sapply(1:nrow(trips), function(i) gsub(paste0(a$trip_id[i], "-"), "", a$id[i]))
    if (!is.null(trips$trip_headsign)) a$headsign = trips$trip_headsign
    if (!is.null(trips$trip_short_name)) a$short_name = trips$trip_short_name
    if (!is.null(trips$trip_long_name)) a$long_name = trips$trip_long_name
    if (!is.null(trips$direction_id)) a$direction_id = trips$direction_id
    if (!is.null(trips$block_id)) a$block_id = trips$block_id
    if (!is.null(trips$shape_id)) a$shape_id = trips$shape_id
    if (!is.null(trips$wheelchair_accessible)) a$wheelchair_accessible = trips$wheelchair_accessible
    if (!is.null(trips$bikes_allowed)) a$bikes_allowed = trips$bikes_allowed

    dbGetQuery(con,
      sprintf("INSERT INTO trips (%s) VALUES ('%s')",
              paste(names(a), collapse = ","),
              do.call(paste, c(a, list(sep = "','", collapse = "'),('")))))
  }



  ## --------------------------------------------------------------- SHAPES
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/shapes.txt" -o shapes.csv')
  shapes = read.csv("shapes.csv")
  unlink("shapes.csv")
  curAg = dbGetQuery(con, "SELECT id FROM shapes")
  new = which((! shapes$shape_id %in% curAg$id) &
              grepl(paste0("-", versions$version, collapse="|"), shapes$shape_id))
  if (length(new) > 0) {
    shapes = shapes[new, ]
    a = list(
      id = shapes$shape_id,
      shape_id = gsub(paste0("-", versions$version, collapse="|"), "", shapes$shape_id),
      lat = shapes$shape_pt_lat,
      lon = shapes$shape_pt_lon,
      pt_sequence = shapes$shape_pt_sequence
    )
    a$version = sapply(1:nrow(shapes), function(i) gsub(paste0(a$shape_id[i], "-"), "", a$id[i]))
    # if (!is.null(shapes$shape_dist_traveled)) a$dist_traveled = shapes$shape_dist_traveled

    shapeids = unique(shapes$shape_id)
    pb <- txtProgressBar(0, length(shapeids), 0, style = 3)
    for (i in 1:length(shapeids)) {
      setTxtProgressBar(pb, i)
      aa = lapply(a, function(x) x[shapes$shape_id == shapeids[i]])
      dbGetQuery(con,
        sprintf("INSERT INTO shapes (%s) VALUES ('%s')",
                paste(names(a), collapse = ","),
                do.call(paste, c(aa, list(sep = "','", collapse = "'),('")))))
    }
    close(pb)
  }

  ## --------------------------------------------------------------- STOPS
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/stops.txt" -o stop.csv')
  stops = read.csv("stop.csv")
  unlink("stop.csv")
  curAg = dbGetQuery(con, "SELECT id FROM stops")
  new = which((! stops$stop_id %in% curAg$id))
  if (length(new) > 0) {
    stops = stops[new, ]
    a = list(
      id = stops$stop_id,
      name = gsub("'", "''", stops$stop_name),
      lat = stops$stop_lat,
      lon = stops$stop_lon
    )
    # if (!is.null(stops$stop_code)) a$code = stops$stop_code
    # if (!is.null(stops$stop_desc)) a$desc = stops$stop_desc
    # if (!is.null(stops$zone_id)) a$zone_id = stops$zone_id
    # if (!is.null(stops$stop_url)) a$url = stops$stop_url
    # if (!is.null(stops$location_type)) a$location_type = stops$location_type
    # if (!is.null(stops$parent_station)) a$parent_station = stops$parent_station
    # if (!is.null(stops$timezone)) a$timezone = stops$timezone
    # if (!is.null(stops$wheelchair_boarding)) a$wheelchair_boarding = stops$wheelchair_boarding

    dbGetQuery(con,
      sprintf("INSERT INTO stops (%s) VALUES ('%s')",
              paste(names(a), collapse = ","),
              do.call(paste, c(a, list(sep = "','", collapse = "'),('")))))
  }


  ## --------------------------------------------------------------- STOP TIMES
  system('curl -s -X GET "https://cdn01.at.govt.nz/data/stop_times.txt" -o stop.csv')
  stops = read.csv("stop.csv")
  unlink("stop.csv")
  curAg = dbGetQuery(con, "SELECT trip_id FROM stop_times")
  new = which((! stop$trip_id %in% curAg$trip_id) &
              grepl(paste0("-", versions$version, collapse="|"), stops$trip_id))
  if (length(new) > 0) {
    stops = stops[new, ]
    a = list(
      trip_id = stops$trip_id,
      arrival_time = stops$arrival_time,
      departure_time = stops$departure_time,
      stop_id = stops$stop_id,
      stop_sequence = stops$stop_sequence
    )
    # if (!is.null(stops$stop_headsign)) a$stop_headsign = stops$stop_headsign
    # if (!is.null(stops$pickup_type)) a$pickup_type = stops$pickup_type
    # if (!is.null(stops$drop_off_type)) a$drop_off_type = stops$drop_off_type
    # if (!is.null(stops$shape_dist_traveled)) a$shape_dist_traveled = stops$shape_dist_traveled
    # if (!is.null(stops$timepoint)) a$timepoint = stops$timepoint

    # tripids = unique(stops$trip_id)
    # pb <- txtProgressBar(0, length(tripids), 0, style = 3)
    # for (i in 1:length(tripids)) {
      # setTxtProgressBar(pb, i)
      # aa = lapply(a, function(x) x[stops$trip_id == tripids[i]])
      dbGetQuery(con,
        sprintf("INSERT INTO stop_times (%s) VALUES ('%s')",
                paste(names(a), collapse = ","),
                do.call(paste, c(a, list(sep = "','", collapse = "'),('")))))
    # }
    # close(pb)
  }
}
