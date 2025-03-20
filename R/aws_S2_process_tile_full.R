aws_S2_process_full_tile <- function(path, prefix = 's2', export_path=NULL, n_cores=NULL){
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
      B03 <- terra::rast(file.path(scene, 'R10m', 'B03.jp2'))
      B04 <- terra::rast(file.path(scene, 'R10m', 'B04.jp2'))
      B08 <- terra::rast(file.path(scene, 'R10m', 'B08.jp2'))
      SCL <- terra::rast(file.path(scene, 'R20m', 'SCL.jp2'))
      SCL <- terra::disagg(SCL, 2)
      
      rst <- c(B03, B04, B08, SCL)
      names(rst) <- paste0(c('B03_', 'B04_', 'B08_', 'SCL_'), format_name_bands)
      
      pizzR::setcreate.wd(export_path, verbose = F)
      pizzR::writeslimRaster(rst, sprintf('%s_%s_%s.tif', prefix, tile, format_name), datatype = 'INT2U', verbose = F)
      gc(reset = T, full = T, verbose = F)
    }
    parallel::stopCluster(cl)
  }
}
