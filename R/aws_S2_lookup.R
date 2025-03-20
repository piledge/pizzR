aws_S2_lookup <- function(tile, years, months, days, verbose = T){
  aws_cli <- Sys.which("aws")
  if (aws_cli == "") stop("AWS-CLI not found. Please install it from 'https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html'")

  scene_dates <- function(years, months, days) {
    days_of_month <- function(year, month) {
      start <- as.Date(sprintf("%04d-%02d-01", year, month))
      if (month == 12) {
        next_month <- as.Date(sprintf("%04d-01-01", year + 1))
      } else {
        next_month <- as.Date(sprintf("%04d-%02d-01", year, month + 1))
      }
      as.integer(next_month - start)
    }

    res <- data.frame(year = integer(), month = integer(), day = integer())
    year_month <- expand.grid(year = years, month = months)

    for (i in seq_len(nrow(year_month))) {
      current_year <- year_month$year[i]
      current_month <- year_month$month[i]
      dom <- days_of_month(current_year, current_month)

      valid_days <- intersect(seq(from = min(days), to = max(days)), seq(1, dom))
      if (length(valid_days) > 0) {
        res <- rbind(res, data.frame(
          year  = rep(current_year, length(valid_days)),
          month = rep(current_month, length(valid_days)),
          day   = valid_days
        ))
      }
    }
    res <- res[order(res$year, res$month, res$day), ]

    res$dates <- sprintf("%04d-%02d-%02d", res$year, res$month, res$day)
    return(res)
  }


  s2_checktiles <- function(tile, years) {
    tile_1 <- substr(tile, 1, 2)
    tile_2 <- substr(tile, 3, 3)
    tile_3 <- substr(tile, 4, 5)
    files <- NULL
    for (i in seq_along(years)) {
      cmd <- sprintf("aws s3 ls s3://sentinel-s2-l2a/tiles/%s/%s/%s/%d/ --no-sign-request --recursive",
                     tile_1, tile_2, tile_3, years[i])
      aws_query <- suppressWarnings(system(cmd, intern = TRUE))
      files <- c(files, aws_query[grep(".jp2", aws_query)])
    }

    split_files <- do.call(rbind, strsplit(files, "tiles"))[, 2]
    split_files <- do.call(rbind, strsplit(split_files, "/"))

    dates <- data.frame(
      year  = as.numeric(split_files[, 5]),
      month = as.numeric(split_files[, 6]),
      day  = as.numeric(split_files[, 7]),
      stringsAsFactors = FALSE
    )

    dates$dates <- sprintf("%04d-%02d-%02d", dates$year, dates$month, dates$day)
    res <- dates[!duplicated(dates$dates), ]
    res <- res[order(res$year, res$month, res$day), ]

    return(res)
  }

  if (verbose) cat(sprintf('%s: Searching AWS for available scenes ...\n', pizzR::Systime()))

  search_dates <- scene_dates(years=years, months = months, days = days)
  available_dates <- s2_checktiles(tile, years)

  requested_scenes <- intersect(search_dates$dates, available_dates$dates)
  split_dates <- do.call(rbind, strsplit(requested_scenes, "-"))
  res <- data.frame(years = as.numeric(split_dates[, 1]), month = as.numeric(split_dates[, 2]), day = as.numeric(split_dates[, 3]), dates = requested_scenes)

  return(res)
}
