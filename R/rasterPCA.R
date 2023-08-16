rst.pca <- function(x,scale=T){

  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("raster", "terra"))
  suppressMessages(pizzR::change.terraOptions())
  
  rsttype <- class(x)[1]
  if (rsttype != "SpatRaster") return(warning("Not a suitable rasterfile!\n"))
  
  band.nr <- terra::nlyr(rst)
  
  rst.mask <- is.na(terra::values(x[[1]]))
  rst.numbered <- seq(rst.mask)
  rst.numbered.masked <- rst.numbered[rst.mask]
  rst.numbered.forest <- rst.numbered[!rst.mask]
  
  pc.data <- terra::values(x)
  pc.data <- matrix(pc.data[rst.numbered.forest,], ncol = band.nr, byrow = F)
  
  if (scale) pc.data <- scale(pc.data, center = T, scale = T)
  if (!scale) pc.data <- scale(pc.data, center = F, scale = F)
  
  pca <- prcomp(pc.data)
  pc.values <- data.frame(pca$x)
  
  na.px <- data.frame(matrix(rep(NA, band.nr * length(rst.numbered.masked)), ncol = band.nr))
  colnames(na.px) <- colnames(pc.values)
  na.px <- cbind(na.px, rst.numbered.masked)
  
  pc.values <- cbind(pc.values, rst.numbered.forest)
  colnames(pc.values)[ncol(pc.values)] <- colnames(na.px)[ncol(na.px)] <- 'position'
  
  merged <- rbind(pc.values, na.px)
  
  pca.rst <- merged[order(merged$position), seq(band.nr)]
  terra::values(x) <- pca.rst

  terra::setMinMax(pca.rst)
  names(x) <- paste0('PC', seq(band.nr))
  
  return(x)
}
