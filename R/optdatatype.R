opt.datatype <- function(x, samplesize=100){
  
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "Rcpp", "terra"))
  
  rsttype <- class(x)[1]
  
  if (samplesize <= 0) return(warning("Samplesize must be >0!\n"))
  if (terra::ncell(x) < samplesize) size <- terra::ncell(x) else size <- samplesize
  
  if (rsttype == "SpatRaster") rst_sample_cells <- terra::spatSample(x, size, "random", values = TRUE, na.rm = TRUE, warn = FALSE)
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack") rst_sample_cells <- as.data.frame(raster::sampleRandom(x, size, na.rm = TRUE))
  
  if (rsttype == "SpatRaster") NA_sample_cells <- terra::spatSample(x, size, "random", values = TRUE, na.rm = FALSE, warn = FALSE)
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack") NA_sample_cells <- as.data.frame(raster::sampleRandom(x, size, na.rm = FALSE))
  
  if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
  
  rst_sample_cells[rst_sample_cells==Inf] <- NA
  contain_NA <- any(is.na(NA_sample_cells))
  
  if (rsttype == "SpatRaster"){         
    rst_min <- min(x@ptr[["range_min"]])
    rst_max <- max(x@ptr[["range_max"]])
    
    if (is.na(rst_min) || is.na(rst_max)){                                      #slower; if ptr not available
      rst.summary <- terra::summary(x, warn = F)
      
      rst_min <- rst.summary[1,]
      rst_min <- unlist(strsplit(rst_min, ":"))
      rst_min <- rst_min[-grep("Min", rst_min)]
      rst_min <- min(as.numeric(rst_min))
      
      rst_max <- rst.summary[6,]
      rst_max <- unlist(strsplit(rst_max, ":"))
      rst_max <- rst_max[-grep("Max", rst_max)]
      rst_max <- min(as.numeric(rst_max))
    }
  }
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){
    rst_min <- min(minValue(x))
    rst_max <- max(maxValue(x))
    
    if (is.na(rst_min) || is.na(rst_max)){                                      #slower; if ptr not available
      rst.summary <- raster::summary(x, maxsamp = 10000)

      rst_min <- min(rst.summary[rownames(rst.summary) == "Min.", ])
      rst_max <- max(rst.summary[rownames(rst.summary) == "Max.", ])
    }
  }
  
  rst_significant_value <- max(abs(c(rst_min, rst_max)))
  rst_signed <- rst_min < 0
  
  rst_float <- TRUE
  if (all((floor(rst_sample_cells) / rst_sample_cells) == 1, na.rm = T)) rst_float <- FALSE

  # for INTU NAflag is the highest value, for INTU NAflag is the lowest value
  INT1U <- c(0, 255)
  INT2U <- c(0, 65534)
  INT4U <- c(0, 4294967296)
  INT1S <- c(-127, 127)
  INT2S <- c(-32767, 32767)
  INT4S <- c(-2147483647, 2147483647)
  FLT4S <- c(-3.4e+38, 3.4e+38)
  FLT8S <- c(-1.7e+308, 1.7e+308)
  
  if (nrow(rst_sample_cells) == 0) return("LOG1S")                              #LOG
  if (is.logical(rst_sample_cells[,1:ncol(rst_sample_cells)])) return("LOG1S")
  
  if (rst_float){                                                               #FLOAT
    if (rst_significant_value < 3.4e+38){
      return(list(datatype = 'FLT4S', NAflag = contain_NA))
    }else{
      return(list(datatype = 'FLT8S', NAflag = contain_NA))
    }
  }
  
  if (!rst_float){                                                              #INT
    if (rst_signed){  
      if (rst_significant_value <= INT2S[2]){
        if (contain_NA) return(list(datatype = 'INT4S', NAflag = contain_NA))
        if (!contain_NA) return(list(datatype = 'INT2S', NAflag = contain_NA))
      }
      if (rst_significant_value <= INT4S[2]){
        if (contain_NA) return(list(datatype = 'FLT4S', NAflag = contain_NA))
        if (!contain_NA) return(list(datatype = 'INT4S', NAflag = contain_NA))
      }
    }
    
    if (!rst_signed){                                                           #INTU
      if (rst_significant_value <= INT1U[2]){
        if (contain_NA) return(list(datatype = 'INT2U', NAflag = contain_NA))
        if (!contain_NA) return(list(datatype = 'INT1U', NAflag = contain_NA))
      }
      if (rst_significant_value <= INT2U[2]){
        if (contain_NA) return(list(datatype = 'INT4U', NAflag = contain_NA))
        if (!contain_NA) return(list(datatype = 'INT2U', NAflag = contain_NA))
      }
      if (rst_significant_value <= INT4U[2]){
        if (contain_NA) return(list(datatype = 'FLT4S', NAflag = contain_NA))
        if (!contain_NA) return(list(datatype = 'INT4U', NAflag = contain_NA))
      }
    }
    
    if (rst_significant_value <= FLT4S[2]) return(list(datatype = 'FLT4S', NAflag = contain_NA))
    return(list(datatype = 'FLT4S', NAflag = contain_NA))
  }
}
