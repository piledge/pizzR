aws_S2_download_tile <- function(tile, year, month, day, cloud_cover=100, d_path=NULL){
  aws_cli <- Sys.which("aws")
  if (aws_cli == "") stop("AWS-CLI not found. Please install it from 'https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html'")

  if (is.null(d_path)) d_path <- tempdir()

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

  cat(sprintf("\n%s: Files will be downloaded to '%s' ...", pizzR::Systime(), d_path))
  cat(sprintf('\n%s: Check Metadata ...\n', pizzR::Systime()))
  system(metadata_aws_cp)
  doc <- xml2::read_xml(file.path(local_folder, 'metadata.xml'))
  cc <- xml2::xml_text(xml2::xml_find_first(doc, "//CLOUDY_PIXEL_PERCENTAGE"))

  if(as.numeric(cc) < cloud_cover){
    cat(sprintf('\n%s: Starting to download tile %s ...\n', pizzR::Systime(), tile_folder))
    system(aws_cp)
    cat(sprintf('%s: Done ...\n', pizzR::Systime()))
    status <- "downloaded"
    final_folder <- local_folder
  } else {
    final_folder <- sprintf('%s_skipped', local_folder)
    if(dir.exists(final_folder)) unlink(final_folder, recursive = TRUE)
    invisible(file.rename(from = local_folder, to = final_folder))
    cat(sprintf('%s: Skipped\n', pizzR::Systime()))
    status <- "skipped"
  }

  dest_dir_downloaded <- file.path(d_path, tile, year, "downloaded")
  dest_dir_skipped <- file.path(d_path, tile, year, "skipped")
  if(!dir.exists(dest_dir_downloaded)) dir.create(dest_dir_downloaded, recursive = TRUE)
  if(!dir.exists(dest_dir_skipped)) dir.create(dest_dir_skipped, recursive = TRUE)

  dest_dir <- if(status == "downloaded") dest_dir_downloaded else dest_dir_skipped
  dest_folder <- file.path(dest_dir, basename(final_folder))
  if(dir.exists(dest_folder)) unlink(dest_folder, recursive = TRUE)
  invisible(file.rename(final_folder, dest_folder))
}
