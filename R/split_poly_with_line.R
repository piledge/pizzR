split_poly_with_line <- function(poly_path, roads_path, out_name=NULL){
  pizzR::package.install(c('lwgeom', 'sf', 'terra'))

  poly  <- sf::st_read(poly_path, quiet = T)
  roads <- sf::st_read(roads_path, quiet = T)

  roads <- sf::st_transform(roads, sf::st_crs(poly))
  poly  <- sf::st_make_valid(poly)
  roads <- sf::st_make_valid(roads)

  terra::plot(terra::vect(poly[1]), col = 'blue', main = 'Input Data')
  terra::plot(terra::vect(roads), add=T, col = 'red', lwd = 2)
  Sys.sleep(5)

  roads_u            <- sf::st_union(roads)
  poly_split         <- lwgeom::st_split(poly, roads_u)
  poly_split         <- sf::st_collection_extract(poly_split, "POLYGON")
  poly_split$orig_id <- poly$ID[poly_split$match_id]

  terra::plot(terra::vect(poly_split), col = 'green', lwd = 2, main = 'Result')

  if (is.null(out_name)) out_name <- sub('.shp', '_split.shp', poly_path)
  pizzR::setcreate.wd(file.path(out_name), verbose = F)
  st_write(poly_split,   out_name, delete_layer = T, quiet = T)

  col_start <- "\033[32m"
  col_end   <- "\033[39m"
  bold_on  <- "\033[1m"
  bold_off <- "\033[22m"
  if (file.exists(out_name)) cat(sprintf('\n%sFile saved to "%s%s%s"%s\n', col_start, bold_on, out_name, bold_off, col_end))
}
