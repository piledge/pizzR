aws_S2_download_tile <- function(tile, year, month, day, cloud_cover, d_path='C:/temp/'){
  
  tile_1 <- substr(tile, 1, 2)
  tile_2 <- substr(tile, 3, 3)
  tile_3 <- substr(tile, 4, 5)
  
  tile_folder <- sprintf('%04d-%02d-%02d', year, month, day)

  local_folder <- file.path(d_path, tile, year, tile_folder)
  if(!dir.exists(local_folder)) dir.create(local_folder, recursive = TRUE)
  
  aws_ls <- sprintf('aws s3 ls s3://sentinel-s2-l2a/tiles/%s/%s/%s/%s/%s/%s/ --no-sign-request', 
                    tile_1, tile_2, tile_3, year, month, day)
  tile_exists <- suppressWarnings(system(aws_ls, intern = TRUE))
  s_trim <- trimws(tile_exists)
  folder_id <- sub(".*PRE ([0123456789])/?$", "\\1", s_trim)
  folder_id <- folder_id[length(folder_id)]
  
  metadata_aws_cp <- sprintf('aws s3 cp s3://sentinel-s2-l2a/tiles/%s/%s/%s/%d/%s/%s/%s/metadata.xml %s/metadata.xml --no-sign-request',
                             tile_1, tile_2, tile_3, year, month, day, folder_id, local_folder)
  aws_cp <- sprintf('aws s3 cp "s3://sentinel-s2-l2a/tiles/%s/%s/%s/%d/%d/%d/%s/" "%s" --recursive --no-sign-request',
                    tile_1, tile_2, tile_3, year, month, day, folder_id, local_folder)
  
  cat(sprintf('\nStarting to Download tile %s ...\n', tile_folder))
  system(metadata_aws_cp)
  doc <- xml2::read_xml(file.path(local_folder, 'metadata.xml'))
  cc <- xml2::xml_text(xml2::xml_find_first(doc, "//CLOUDY_PIXEL_PERCENTAGE"))

  if(as.numeric(cc) < cloud_cover){
    system(aws_cp)
    cat('Done ...\n')
    status <- "downloaded"
    final_folder <- local_folder
  } else {
    final_folder <- sprintf('%s_skipped', local_folder)
    if(dir.exists(final_folder)) unlink(final_folder, recursive = TRUE)
    file.rename(from = local_folder, to = final_folder)
    cat('\nSkipped ...\n')
    status <- "skipped"
  }

  dest_dir_downloaded <- file.path(d_path, tile, year, "downloaded")
  dest_dir_skipped <- file.path(d_path, tile, year, "skipped")
  if(!dir.exists(dest_dir_downloaded)) dir.create(dest_dir_downloaded, recursive = TRUE)
  if(!dir.exists(dest_dir_skipped)) dir.create(dest_dir_skipped, recursive = TRUE)

  dest_dir <- if(status == "downloaded") dest_dir_downloaded else dest_dir_skipped
  file.rename(final_folder, file.path(dest_dir, basename(final_folder)))
}
