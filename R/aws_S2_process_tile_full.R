aws_S2_process_tile_full <- function(path, prefix = 's2', allbands=FALSE, scl=TRUE, export_path=NULL, n_cores=2){
  if (is.null(export_path)) export_path <- file.path(path, 'export')

  tile_folders <- list.dirs(file.path(path), full.names = T, recursive = F)
  tile_folders <- tile_folders[!grepl('export', tile_folders)]

  for (i in seq(tile_folders))  {

    tile <- basename(tile_folders[i])
    year_folders <- list.dirs(tile_folders[i], recursive = F, full.names = T)
    year_folders_sub <- list.dirs(year_folders, recursive = F, full.names = T)
    downloaded_folders <- year_folders_sub[-grep('skipped', year_folders_sub)]
    scene_folders <- list.dirs(downloaded_folders, recursive = F, full.names = T)

    library(foreach)
    if (is.null(n_cores)) n_cores <- parallelly::availableCores() - 1
    if (n_cores > 10) n_cores <- 10

    cl <- parallelly::makeClusterPSOCK(n_cores, autoStop = TRUE)
    doParallel::registerDoParallel(cl)

    foreach::foreach(i=seq(scene_folders)) %dopar% {
      Sys.sleep(sample(30, 1))

      scene <- scene_folders[i]

      format_name <- basename(scene)
      format_name_bands <- gsub('-', '', basename(scene))
      rst_B02 <- terra::rast(file.path(scene, 'R10m', 'B02.jp2'))
      rst_B03 <- terra::rast(file.path(scene, 'R10m', 'B03.jp2'))
      rst_B04 <- terra::rast(file.path(scene, 'R10m', 'B04.jp2'))
      rst_B08 <- terra::rast(file.path(scene, 'R10m', 'B08.jp2'))

      if (allbands){
        rst_B05 <- terra::rast(file.path(scene, 'R20m', 'B05.jp2'))
        rst_B06 <- terra::rast(file.path(scene, 'R20m', 'B06.jp2'))
        rst_B07 <- terra::rast(file.path(scene, 'R20m', 'B07.jp2'))
        rst_B8A <- terra::rast(file.path(scene, 'R20m', 'B8A.jp2'))
        rst_B11 <- terra::rast(file.path(scene, 'R20m', 'B11.jp2'))
        rst_B12 <- terra::rast(file.path(scene, 'R20m', 'B12.jp2'))
        rst_B01 <- terra::rast(file.path(scene, 'R60m', 'B01.jp2'))
        rst_B09 <- terra::rast(file.path(scene, 'R60m', 'B09.jp2'))
        rst_B05 <- terra::disagg(rst_B05, 2)
        rst_B06 <- terra::disagg(rst_B06, 2)
        rst_B07 <- terra::disagg(rst_B07, 2)
        rst_B8A <- terra::disagg(rst_B8A, 2)
        rst_B11 <- terra::disagg(rst_B11, 2)
        rst_B12 <- terra::disagg(rst_B12, 2)
        rst_B01 <- terra::disagg(rst_B01, 6)
        rst_B09 <- terra::disagg(rst_B09, 6)
        rst <- c(rst_B01, rst_B02, rst_B03, rst_B04, rst_B05, rst_B06, rst_B07, rst_B08, rst_B8A, rst_B09, rst_B11, rst_B12)
        names(rst) <- paste0(c('B01_', 'B02_', 'B03_', 'B04_', 'B05_', 'B06_', 'B07_', 'B08_', 'B8A', 'B09', 'B11', 'B12'), format_name_bands)
      }else{
        rst <- c(rst_B02, rst_B03, rst_B04, rst_B08)
        names(rst) <- paste0(c('B02_', 'B03_', 'B04_', 'B08_'), format_name_bands)
      }

      if (scl){
        rst_SCL <- terra::rast(file.path(scene, 'R20m', 'SCL.jp2'))
        rst_SCL <- terra::disagg(rst_SCL, 2)
        names(rst_SCL) <-  paste0(c('SCL_'), format_name_bands)
        rst <- c(rst, rst_SCL)
      }

      pizzR::setcreate.wd(export_path, verbose = F)
      pizzR::writeslimRaster(rst, sprintf('%s_%s_%s.tif', prefix, tile, format_name), datatype = 'INT2U', verbose = F)
      gc(reset = T, full = T, verbose = F)
    }
    parallel::stopCluster(cl)
  }
}
