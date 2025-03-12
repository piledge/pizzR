aws_S2_check <- function(tile, years, month, day){
  scene_dates <- function(years, month){
    days_of_month <- function(year, month) {
      start <- as.Date(sprintf("%04d-%02d-01", year, month))
      if (month == 12) {
        next_month <- as.Date(sprintf("%04d-01-01", year + 1))
      } else {
        next_month <- as.Date(sprintf("%04d-%02d-01", year, month + 1))
      }
      days <- as.integer(next_month - start)
      return(days)
    }
    
    
    
    res <- data.frame(year=NULL, month=NULL, day=NULL)
    year_month <- data.frame(year=rep(years, each = length(month)), month=rep(month, length(years)))
    for (i in seq(nrow(year_month))){
      dom <- days_of_month(year_month$year[i], year_month$month[i])
      seq_days <- seq(from = min(day), to = max(day))
      seq_month <- seq(days_of_month(year_month$year[i], year_month$month[i]))
      days <- intersect(seq_days, seq_month)
      
      res <- rbind(res, data.frame(year=rep(year_month$year[i], length(days)), month=rep(year_month$month[i], day=length(days)), days))
    }
    res$dates <- sprintf("%04d-%02d-%02d", res$year, res$month, res$days)
    return(res)
  }
  
  s2_checktiles <- function(tile, years){
    tile_1 <- substr(tile, 1,2)
    tile_2 <- substr(tile, 3,3)
    tile_3 <- substr(tile, 4,5)
    
    files <- NULL
    for (i in seq(length(years))){
      cmd <- sprintf('aws s3 ls s3://sentinel-s2-l2a/tiles/%s/%s/%s/%d/ --no-sign-request --recursive', tile_1, tile_2, tile_3, years[i])
      aws_query <- suppressWarnings(system(cmd, intern = T))
      files <- c(files, (aws_query[grep('.jp2', aws_query)]))
    }
    
    split_files <- do.call(rbind, strsplit(files, "tiles"))[,2]
    split_files <- do.call(rbind, strsplit(split_files, "/"))
    dates <- data.frame(year = as.numeric(split_files[,5]), month = as.numeric(split_files[,6]), days = as.numeric(split_files[,7]), stringsAsFactors = FALSE)
    dates$dates <- sprintf("%04d-%02d-%02d", dates$year, dates$month, dates$days)
    res <- dates[!duplicated(dates$dates), ]
    
    return(res)
  }
  
  search_dates <- scene_dates(years, month)
  available_dates <- s2_checktiles(tile, years)
  requested_scenes <- intersect(search_dates$dates, available_dates$dates)
  split_dates <- do.call(rbind, strsplit(requested_scenes, "-"))
  res <- data.frame(years=as.numeric(split_dates[,1]), month=as.numeric(split_dates[,2]), day=as.numeric(split_dates[,3]), dates=requested_scenes)
  
  return(res)
}