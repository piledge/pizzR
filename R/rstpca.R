pizzR::change.terraOptions()

rfiles <- list.files('C:/Users/Michael/Desktop/segmentation', full.names = T, pattern = '.tif$')


rst <- rast(rfiles[2])
plot(rst)



rst.pca <- function(x,scale=T){
  
  rst.mask <- is.na(terra::values(x[[1]]))
  rst.numbered <- seq(rst.mask)
  rst.numbered.masked <- rst.numbered[rst.mask]
  rst.numbered.forest <- rst.numbered[!rst.mask]
  
  pc.data <- terra::values(x)
  pc.data <- matrix(pc.data[rst.numbered.forest,], ncol = 3, byrow = F)

  if (scale) pc.data <- scale(pc.data, center = T, scale = T)
  
  
  pca <- prcomp(pc.data)
  
  pc.values <- data.frame(pca$x)
  
  na.px <- data.frame(matrix(rep(NA, ncol(pc.values) * length(rst.numbered.masked)), ncol = ncol(pc.values)))
  colnames(na.px) <- colnames(pc.values)
  na.px <- cbind(na.px, rst.numbered.masked)
  
  pc.values <- cbind(pc.values, rst.numbered.forest)
  colnames(pc.values)[ncol(pc.values)] <- colnames(na.px)[ncol(na.px)] <- 'position'
  
  merged <- rbind(pc.values, na.px)
  
  pca.rst <- merged[order(merged$position), c(1:(ncol(merged)-1))]
  terra::values(x) <- pca.rst
  
  return(x)
}

huhu <- rst.pca(rst, scale = F)
plot(x)
