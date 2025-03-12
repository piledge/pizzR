aws_S2_download_tile <- function(tile, year, month, day, cloud_cover, d_path='C:/temp/'){

  tile_1 <- substr(tile, 1,2)
  tile_2 <- substr(tile, 3,3)
  tile_3 <- substr(tile, 4,5)
  
  tile_folder <- sprintf('%04d-%02d-%02d', year, month, day)
  aws_ls <- sprintf('aws s3 ls s3://sentinel-s2-l2a/tiles/%s/%s/%s/%s/%s/%s/ --no-sign-request', tile_1, tile_2, tile_3, year, month, day)
  tile_exists <- suppressWarnings(system(aws_ls, intern = T))
  s_trim <- trimws(tile_exists)
  folder_id <- sub(".*PRE ([0123456789])/?$", "\\1", s_trim)
  folder_id <- folder_id[length(folder_id)]
  
  metadata_aws_cp <- sprintf('aws s3 cp s3://sentinel-s2-l2a/tiles/%s/%s/%s/%d/%s/%s/%s/metadata.xml %s/%s/%s/metadata.xml --no-sign-request',
                             tile_1, tile_2, tile_3, year, month, day, folder_id, d_path, tile, tile_folder)
  aws_cp <- sprintf('aws s3 cp "s3://sentinel-s2-l2a/tiles/%s/%s/%s/%d/%d/%d/%s/" "%s/%s/%s" --recursive --no-sign-request',
                    tile_1, tile_2, tile_3, year, month, day, folder_id, d_path, tile, tile_folder)
  
  cat(sprintf('\nStarting to Download tile %s ...\n', i))
  system(metadata_aws_cp)
  doc <- xml2::read_xml(file.path(d_path, tile, tile_folder, 'metadata.xml'))
  cc <- xml2::xml_text(xml2::xml_find_first(doc, "//CLOUDY_PIXEL_PERCENTAGE"))

  if (cc < cloud_cover){
    system(aws_cp)
    cat('Done ...\n')
  }else{
    rename_folder <- file.path(d_path, tile, tile_folder)
    rename_folder_new <- sprintf('%s_skipped', rename_folder)
    if (dir.exists(rename_folder_new)) unlink(rename_folder_new, recursive = TRUE)
    file.rename(from=rename_folder, to=rename_folder_new)
    cat('\nSkipped ...\n')
  }
}
