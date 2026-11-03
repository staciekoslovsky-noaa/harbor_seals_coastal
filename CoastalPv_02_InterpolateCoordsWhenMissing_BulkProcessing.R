# Coastal Pv Surveys: Interpolate coordinates for images when missing (bulk)
# S. Koslovsky, 04MAY2017

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgres")
install_pkg("DBI")
install_pkg("geosphere")
install_pkg("lubridate")

# Run code -------------------------------------------------------
# Connect to PostgreSQL ---------------------------------------------------------------
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("mml_pep_db"),
  host = Sys.getenv("mml_pep_ip"),
  user = Sys.getenv("mml_pep_user"),
  password = Sys.getenv("mml_pep_user_pw")
)

# Get data that need to be processed --------------------------------------------------
track_to_interp <- RPostgres::dbGetQuery(
  con,
  "SELECT DISTINCT photog_date_id FROM surv_pv_cst.proc_images_to_interp WHERE photog_date_id = 'HLZ_20260807'"
)
#LIKE \'%2026%\' ")

for (j in 1:nrow(track_to_interp)) {
  image_id <- track_to_interp$photog_date_id[j]
  track_id <- as.character(RPostgres::dbGetQuery(
    con,
    paste(
      "SELECT trackid FROM surv_pv_cst.tbl_track_by_photog WHERE photog_date_id = \'",
      image_id,
      "\'",
      sep = ""
    )
  ))

  # Get data that need to be processed --------------------------------------------------
  img <- dbGetQuery(
    con,
    paste(
      "SELECT * FROM surv_pv_cst.proc_images_to_interp WHERE photog_date_id = \'",
      image_id,
      "\' ORDER BY id",
      sep = ""
    )
  )
  img$original_dt <- lubridate::ymd_hms(
    img$original_dt,
    tz = "America/Vancouver"
  )
  img$gps_dt <- lubridate::ymd_hms(img$gps_dt, tz = "America/Vancouver")

  if (nrow(img) == 0) {
    stop("no images missing gps coordinates!")
  }

  # Process individual images to get gps_dt, latidude, longitude, altitude --------------
  pts <- dbGetQuery(
    con,
    paste(
      "SELECT gps_dt, latitude, longitude, altitude FROM surv_pv_cst.geo_track_pts WHERE trackid = \'",
      track_id,
      "\' ORDER BY id",
      sep = ""
    )
  )
  pts$gps_dt <- lubridate::ymd_hms(pts$gps_dt, tz = "America/Vancouver")

  if (substr(track_id, nchar(track_id), nchar(track_id)) == 'X') {
    for (i in 1:nrow(img)) {
      img$gps_dt <- img$original_dt

      RPostgres::dbSendQuery(
        con,
        paste(
          "UPDATE surv_pv_cst.tbl_image_exif SET latitude = 2",
          ", longitude = 2",
          ", altitude = -99",
          ", gps_dt = \'",
          format(
            lubridate::ymd_hms(img$gps_dt[i], tz = "America/Vancouver"),
            tz = "UTC"
          ),
          "\', interpolated_lku = \'Y\' WHERE id = ",
          img$id[i],
          sep = ""
        )
      )
    }
    next()
  }

  if (nrow(pts) == 0) {
    stop(
      "no gps coordinates available for matching...check for another GPS track to use!"
    )
  }
  offset <- as.numeric(dbGetQuery(
    con,
    paste(
      "SELECT median_diff FROM surv_pv_cst.summ_exif_dt WHERE photog_date_id = \'",
      image_id,
      "\'",
      sep = ""
    )
  ))

  for (i in 1:nrow(img)) {
    img$gps_dt[i] <- as.POSIXlt(
      ifelse(
        is.na(img$original_dt[i]),
        NA,
        ifelse(
          length(offset) == 0 | is.na(offset) == TRUE,
          img$original_dt[i],
          img$original_dt[i] + offset
        )
      ),
      tz = "America/Vancouver",
      origin = '1970-01-01'
    )
    for (k in 1:nrow(pts)) {
      pts$timing[k] <- ifelse(
        is.na(img$gps_dt[i]),
        "no_data",
        ifelse(
          img$gps_dt[i] == pts$gps_dt[k],
          "equal",
          ifelse(img$gps_dt[i] > pts$gps_dt[k], "before", "after")
        )
      )
    }
    rm(k)

    if (nrow(subset(pts, timing == "no_data")) == nrow(pts)) {
      RPostgres::dbSendQuery(
        con,
        paste(
          "UPDATE surv_pv_cst.tbl_image_exif SET latitude = 2",
          ", longitude = 2",
          ", altitude = -99",
          ", gps_dt = NULL",
          ", interpolated_lku = \'Y\' WHERE id = ",
          img$id[i],
          sep = ""
        )
      )
    } else if (nrow(subset(pts, timing == "equal")) > 0) {
      coord_equal <- pts[which(pts$timing == "equal"), ]
      RPostgres::dbSendQuery(
        con,
        paste(
          "UPDATE surv_pv_cst.tbl_image_exif SET latitude = ",
          coord_equal$latitude[1],
          ", longitude = ",
          coord_equal$longitude[1],
          ", altitude = ",
          as.integer(coord_equal$altitude[1]),
          ", gps_dt = \'",
          format(
            lubridate::ymd_hms(coord_equal$gps_dt[1], tz = "America/Vancouver"),
            tz = "UTC"
          ),
          "\', interpolated_lku = \'Y\' WHERE id = ",
          img$id[i],
          sep = ""
        )
      )
      rm(coord_equal)
    } else if (nrow(subset(pts, timing == "before")) > 0) {
      if (nrow(subset(pts, timing == "after")) > 0) {
        coord_before <- pts[
          which(row.names(pts) == max(which(pts$timing == "before"))),
        ]
        coord_after <- pts[
          which(row.names(pts) == min(which(pts$timing == "after"))),
        ]
        img_bearing <- geosphere::bearing(
          c(coord_before$longitude[1], coord_before$latitude[1]),
          c(coord_after$longitude[1], coord_after$latitude[1])
        )
        pt_time <- as.numeric(difftime(
          coord_after$gps_dt[1],
          coord_before$gps_dt[1],
          units = "sec"
        ))
        img_time <- as.numeric(difftime(
          img$gps_dt[i],
          coord_before$gps_dt[1],
          units = "sec"
        ))
        img_dist <- geosphere::distHaversine(
          c(coord_before$longitude[1], coord_before$latitude[1]),
          c(coord_after$longitude[1], coord_after$latitude[1])
        ) *
          (img_time / pt_time)
        new_coord <- geosphere::destPoint(
          c(coord_before$longitude[1], coord_before$latitude[1]),
          img_bearing,
          img_dist
        )
        new_lat <- new_coord[2]
        new_long <- new_coord[1]
        new_alt <- coord_before$altitude[1] +
          (coord_after$altitude[1] - coord_before$altitude[1]) *
            (img_time / pt_time)
        new_dt <- format(
          lubridate::ymd_hms(img$gps_dt[i], tz = "America/Vancouver"),
          tz = "UTC"
        )
        RPostgres::dbSendQuery(
          con,
          paste(
            "UPDATE surv_pv_cst.tbl_image_exif SET latitude = ",
            new_lat,
            ", longitude = ",
            new_long,
            ", altitude = ",
            as.integer(new_alt),
            ", gps_dt = \'",
            new_dt,
            "\', interpolated_lku = \'Y\' WHERE id = ",
            img$id[i],
            sep = ""
          )
        )
        rm(
          coord_before,
          coord_after,
          img_bearing,
          pt_time,
          img_time,
          img_dist,
          new_coord,
          new_lat,
          new_long,
          new_alt,
          new_dt
        )
      } else {
        RPostgres::dbSendQuery(
          con,
          paste(
            "UPDATE surv_pv_cst.tbl_image_exif SET latitude = 2",
            ", longitude = 2",
            ", altitude = -99",
            ", gps_dt = \'",
            format(
              lubridate::ymd_hms(img$gps_dt[i], tz = "America/Vancouver"),
              tz = "UTC"
            ),
            "\', interpolated_lku = \'Y\' WHERE id = ",
            img$id[i],
            sep = ""
          )
        )
      }
    } else {
      RPostgres::dbSendQuery(
        con,
        paste(
          "UPDATE surv_pv_cst.tbl_image_exif SET latitude = 2",
          ", longitude = 2",
          ", altitude = -99",
          ", gps_dt = \'",
          format(
            lubridate::ymd_hms(img$gps_dt[i], tz = "America/Vancouver"),
            tz = "UTC"
          ),
          "\', interpolated_lku = \'Y\' WHERE id = ",
          img$id[i],
          sep = ""
        )
      )
    }
  }

  # Recalculate geom field in database --------------------------------------------------
  RPostgres::dbSendQuery(
    con,
    "UPDATE surv_pv_cst.tbl_image_exif SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)"
  )
  write.csv(
    img,
    paste(
      "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_HarborSeal_Coastal\\DB\\InterpolatedImages\\interpolated_",
      image_id,
      ".csv",
      sep = ""
    ),
    row.names = FALSE
  )
}

# Disconnect for database and delete unnecessary variables ----------------------------
RPostgres::dbDisconnect(con)
rm(con, i, img, pts, offset)
