# Coastal Pv Surveys: Load GPS Tracklines
# S. Koslovsky, 03AUG2017

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgres")
install_pkg("DBI")
install_pkg("XML")
install_pkg("dplyr")
install_pkg("geosphere")
install_pkg("lubridate")

# Run code -------------------------------------------------------
# Connect to DB -------------------------------------------------------------------------
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("mml_pep_db"),
  host = Sys.getenv("mml_pep_ip"),
  user = Sys.getenv("mml_pep_user"),
  password = Sys.getenv("mml_pep_user_pw")
)
imported <- RPostgres::dbGetQuery(con, "SELECT DISTINCT gpx_file FROM surv_pv_cst.geo_track_pts")
imported$import <- 'Y'

# Process GPX Files ---------------------------------------------------------------------
trackfiles <- list.files(path = "S://survey_harbor_seals_coastal/2026", pattern = "[^.]+?\\.(gpx)$", recursive = TRUE, full.names = TRUE)
x <- strsplit(trackfiles, "/")
fnames <- sapply(x, function(x){x[length(x)]})
trackfiles <- trackfiles[nchar(fnames) == 16 | nchar(fnames) == 17]
rm(x, fnames)
trackfiles <- trackfiles[grep("[0-9][0-9][0-9][0-9]/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]", trackfiles)]
trackfiles <- trackfiles[grep("[A-Z|a-z][A-Z|a-z][A-Z|a-z]_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]", trackfiles)]

# Subset list of trackfiles to only those that have not been previously imported
trk <- data.frame(matrix(trackfiles), stringsAsFactors = FALSE)
colnames(trk) <- "dir"
trk$file <- basename(trk$dir)
trk$file <- toupper(gsub('.GPX|.gpx', '', trk$file))

trk <- merge(trk, imported, by.x = "dir", by.y = "gpx_file", all = TRUE)
trk <- trk[which(is.na(trk$import)), ]

trackfiles <- as.character(trk$dir)

right <- function(x,k){substr(x, nchar(x)-k+1, nchar(x))}

#RPostgres::dbSendQuery(con, "DELETE FROM surv_pv_cst.geo_track_pts")
RPostgres::dbSendQuery(con, "ALTER TABLE surv_pv_cst.geo_track_pts DROP COLUMN IF EXISTS geom")





# Loop through trackfiles and process to database ---------------------------------------
for(i in 1:length(trackfiles)) {
    trkID <-  as.character(unlist(strsplit(right(trackfiles[i], 17), '\\.'))[1]) 
    trkID <- unlist(strsplit(trkID,"/"))
    trkID <- as.character(trkID[length(trkID)])
    trkID_split <- unlist(strsplit(trkID, "_"))
    trkID <- paste(toupper(trkID_split[1]), '_', trkID_split[2],sep = "")

    # Process track data (to points and lines in the DB)...if the data do not already exist in the DB or if the the record count of the file is not identical to the existing number of records in the DB
    track <- tryCatch(htmlTreeParse(file = trackfiles[i], useInternalNodes = TRUE), error = function(cond)"skip") 

    coords <- XML::xpathSApply(doc = track, path = "//trkpt", fun = xmlAttrs)
    time <- xpathSApply(doc = track, path = "//trkpt/time", fun = xmlValue)
    altitude <- xpathSApply(doc = track, path = "//trkpt/ele", fun = xmlValue)
    
    track <- data.frame(
      coords.x1 = as.numeric(coords["lon", ]),
      coords.x2 = as.numeric(coords["lat", ]),
      time = ymd_hms(time, tz = "UTC"),
      altitude = as.numeric(altitude)
    )
    rm(coords, time, altitude)
    
    if (track[[1,1]] == "skip") {next}
    
    next_id <- as.numeric(RPostgres::dbGetQuery(con, "SELECT max(id) FROM surv_pv_cst.geo_track_pts"))
    
    # Remove records from unexpected dates
    date1 <- as.Date(substr(trkID_split[2], 1, 8), format = "%Y%m%d")
    date2 <- date1 + 1
    track$date <- as.Date(track$time, format = "%Y/%m/%d")
    track <- track[which(track$date == date1 | track$date == date2), ]
    
    # Remove records too far from subsequent points
    ## In starting order
    track$id <- as.integer(row.names(track))
    track <- 
      track %>%
      mutate(lag.lat = lag(coords.x2, 1)) %>%
      mutate(lag.long = lag(coords.x1, 1)) %>%
      mutate(lag.lat = ifelse(is.na(lag.lat), coords.x2, lag.lat)) %>%
      mutate(lag.long = ifelse(is.na(lag.long), coords.x1, lag.long)) 
    track$dist_m = geosphere::distHaversine(track[, c("coords.x1", "coords.x2")], track[, c("lag.long", "lag.lat")])
    track <-
      track %>%
      mutate(cum.total = cumsum(dist_m))
    track <- track[which(track$cum.total != 0), ]
    
    ## In reverse order
    track <- track[order(track$id, decreasing = TRUE), ]
    track <- 
      track %>%
      mutate(lag.lat = lag(coords.x2, 1)) %>%
      mutate(lag.long = lag(coords.x1, 1)) %>%
      mutate(lag.lat = ifelse(is.na(lag.lat), coords.x2, lag.lat)) %>%
      mutate(lag.long = ifelse(is.na(lag.long), coords.x1, lag.long)) 
    track$dist_m = geosphere::distHaversine(track[, c("coords.x1", "coords.x2")], track[, c("lag.long", "lag.lat")])
    track <-
      track %>%
      mutate(cum.total = cumsum(dist_m))
    track <- track[which(track$cum.total != 0), ]
    
    track <- track[order(track$id), ]
    
    
    
    # Process data for DB import
    row.names(track) <- 1:nrow(track)
    if (is.na(next_id) == TRUE) {
      track$id <- as.numeric(row.names(track))
    } else {
      track$id <- next_id + as.numeric(row.names(track))
    }
    track$gpx_file <- stringr::str_remove(trackfiles[i][1], "^.*(?=survey_harbor_seals_coastal)")
    track$trackid <- toupper(trkID)
    track$segmentid <- -99
    track$gps_dt <- ymd_hms(track$time, tz = "UTC")
    track$latitude <- track$coords.x2
    track$longitude <- track$coords.x1
    track$altitude <- ifelse(is.na(track$altitude), -99, track$altitude)
    track <- subset(track, select = c("id", "gpx_file", "trackid", "segmentid", "gps_dt", "latitude", "longitude", "altitude"))
      
    # Insert values into database
    target_table <- Id(schema = "surv_pv_cst", table = "geo_track_pts")
    RPostgres::dbWriteTable(con, target_table, track, overwrite = FALSE, append = TRUE)

    rm(track, trkID, trkID_split, next_id, date1, date2, target_table)
}

# Create and populate geom field
RPostgres::dbSendQuery(con, "ALTER TABLE surv_pv_cst.geo_track_pts ADD COLUMN geom geometry(POINT, 4326)")
RPostgres::dbSendQuery(con, "UPDATE surv_pv_cst.geo_track_pts SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)")

# Process track points to lines
RPostgres::dbSendQuery(con, "DELETE FROM surv_pv_cst.geo_track_lines")
RPostgres::dbSendQuery(con, "INSERT INTO surv_pv_cst.geo_track_lines (SELECT geo_track_pts.trackid, ST_MakeLine(geo_track_pts.geom ORDER BY gps_dt) As geom
            FROM surv_pv_cst.geo_track_pts
            GROUP BY geo_track_pts.trackid)", overwrite = TRUE)

# Disconnect for database and delete unnecessary variables ------------------------------
RPostgres::dbDisconnect(con)
rm(i, con, right)
