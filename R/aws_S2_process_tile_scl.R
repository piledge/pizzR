aws_S2_process_tile_scl <- function(path, tile, shp_path, crop_ext_buffer = NULL, prefix = 's2', mask = F, preview = F){
  terra::terraOptions(verbose=F)
  area <- terra::vect(shp_path)
  area_buffered <- terra::buffer(area, 10)
  crop_ext <- terra::ext(area_buffered)
  crop_region <- if (is.null(crop_ext_buffer)) crop_ext else crop_ext + crop_ext_buffer

  tile_dirs <- list.dirs(file.path(path, tile))
  downloaded_dirs <- tile_dirs[grep('downloaded$', tile_dirs)]
  scene_folders <- list.files(downloaded_dirs, full.names = T)

  n_scenes <- length(scene_folders)
  total_digits <- nchar(n_scenes)

  cat(sprintf('%s: %s scenes found. Start processing ...\n', pizzR::Systime(), n_scenes))
  for (i in seq(scene_folders)){
    pizzR::loop_progress(i, total_digits = total_digits)
    scene_img_paths <- list.files(scene_folders[i], pattern = '.jp2$', recursive = T, full.names = T)

    metadata_path <- list.files(scene_folders[i], pattern = 'metadata.xml', recursive = T, full.names = T)
    metadata <- pizzR::aws_S2_get_metadata(metadata_path)

    rst_scl <- terra::rast(scene_img_paths[grep('/R20m/SCL.jp2', scene_img_paths)])
    terra::crs(rst_scl) <- metadata$EPSG$crs_param
    terra::ext(rst_scl) <- metadata$R20$ext_param_20

    scl_cropped <- terra::crop(rst_scl, crop_ext)
    if(!is.null(mask)) scl_cropped <- terra::mask(scl_cropped, area_buffered)
    if (preview) terra::plot(scl_cropped, main = basename(scene_folders[i]))

    scl_values <- terra::values(scl_cropped)
    if (!any(c(0 %in% scl_values, 3 %in% scl_values, 8 %in% scl_values, 9 %in% scl_values, 10 %in% scl_values))){

      cat(sprintf('%s: Processing file ...\n', pizzR::Systime()))
      rst_B01 <- terra::rast(scene_img_paths[grep('/R60m/B01.jp2', scene_img_paths)])
      rst_B02 <- terra::rast(scene_img_paths[grep('/R10m/B02.jp2', scene_img_paths)])
      rst_B03 <- terra::rast(scene_img_paths[grep('/R10m/B03.jp2', scene_img_paths)])
      rst_B04 <- terra::rast(scene_img_paths[grep('/R10m/B04.jp2', scene_img_paths)])
      rst_B05 <- terra::rast(scene_img_paths[grep('/R20m/B05.jp2', scene_img_paths)])
      rst_B06 <- terra::rast(scene_img_paths[grep('/R20m/B06.jp2', scene_img_paths)])
      rst_B07 <- terra::rast(scene_img_paths[grep('/R20m/B07.jp2', scene_img_paths)])
      rst_B08 <- terra::rast(scene_img_paths[grep('/R10m/B08.jp2', scene_img_paths)])
      rst_B8A <- terra::rast(scene_img_paths[grep('/R20m/B8A.jp2', scene_img_paths)])
      rst_B09 <- terra::rast(scene_img_paths[grep('/R60m/B09.jp2', scene_img_paths)])
      rst_B11 <- terra::rast(scene_img_paths[grep('/R20m/B11.jp2', scene_img_paths)])
      rst_B12 <- terra::rast(scene_img_paths[grep('/R20m/B12.jp2', scene_img_paths)])

      terra::crs(rst_B01) <- terra::crs(rst_B02) <- terra::crs(rst_B03) <- terra::crs(rst_B04) <- terra::crs(rst_B05) <- terra::crs(rst_B06) <- terra::crs(rst_B07) <-
        terra::crs(rst_B08) <- terra::crs(rst_B8A) <- terra::crs(rst_B09) <- terra::crs(rst_B11) <- terra::crs(rst_B12) <- metadata$EPSG$crs_param

      terra::ext(rst_B02) <- terra::ext(rst_B03) <- terra::ext(rst_B04) <- terra::ext(rst_B08) <- metadata$R10$ext_param_10
      terra::ext(rst_B05) <- terra::ext(rst_B06) <- terra::ext(rst_B07) <- terra::ext(rst_B8A) <- terra::ext(rst_B11) <- terra::ext(rst_B12) <- metadata$R20$ext_param_20
      terra::ext(rst_B01) <- terra::ext(rst_B09) <- metadata$R60$ext_param_60

      rst_B05 <- terra::disagg(rst_B05, 2)
      rst_B06 <- terra::disagg(rst_B06, 2)
      rst_B07 <- terra::disagg(rst_B07, 2)
      rst_B8A <- terra::disagg(rst_B8A, 2)
      rst_B11 <- terra::disagg(rst_B11, 2)
      rst_B12 <- terra::disagg(rst_B12, 2)
      rst_scl <- terra::disagg(rst_scl, 2)
      rst_B01 <- terra::disagg(rst_B01, 6)
      rst_B09 <- terra::disagg(rst_B09, 6)

      stacked <- c(rst_B01, rst_B02, rst_B03, rst_B04, rst_B05, rst_B06, rst_B07, rst_B08, rst_B8A, rst_B09, rst_B11, rst_B12, rst_scl)
      stacked_crop <- terra::crop(stacked, crop_region)

      cat(sprintf('%s: Writing file ...\n', pizzR::Systime()))
      pizzR::setcreate.wd(file.path(path, tile, 'export'), verbose = F)
      pizzR::writeslimRaster(stacked_crop, sprintf('%s_%s_%s.tif', prefix, tile, (basename(scene_folders[i]))), datatype = 'INT2U', verbose = F)
      setwd(path)
    }
  }
  cat(sprintf('%s: Processing finished ...\n', pizzR::Systime()))
}
