#' opt.datatype() ##########################################################################
#' returns an optimized export-datatype from a given spatialraster (calculated by use of a pixel-subset)
#' @param spatialraster SpatRaster of terra-package


opt.datatype <- function(spatialraster){

  sample.size <- function(cellnumber, k1 = 0.5, d1 = 15000, k2 = 0.25, d2 = 40000, maxsample = 50000){
    if (k2 * (cellnumber - d2) + k1*(d2-d1)+d1 > maxsample){
      return(maxsample)
    }
    if (cellnumber > d2){
      return(k2 * (cellnumber - d2) + k1*(d2-d1)+d1)
    }else if (cellnumber > d1){
      return(k1 * (cellnumber - d1) + d1)
    }else{
      return(cellnumber)
    }
  }

  sample_size <- sample.size(ncell(spatialraster))


  rst_sample_cells <- abs(spatSample(spatialraster, sample_size, "random", values = T, na.rm = TRUE, warn = F))
  rst_sample_cells[rst_sample_cells==Inf] <- NA

  rst_min <- range(rst_sample_cells)[1]
  rst_max <- range(rst_sample_cells)[2]

  rst_significant_value <- max(abs(c(rst_min, rst_max)))
  rst_signed <- rst_min < 0

  rst_float <- !any(rst_sample_cells / floor(rst_sample_cells) == 1)

  if (nrow(rst_sample_cells) == 0) return("LOG1S")                                  # LOG
  if (is.logical(rst_sample_cells[,1:ncol(rst_sample_cells)])) return("LOG1S")

  if (rst_float == T){                              # FLOAT
    if (rst_significant_value < 3.4e+38){
      return("FLT4S")
    }else
      return("FLT8S")
  }

  if (rst_float == F){                              # INT
    if (rst_signed == T){                             #INTS
      if (rst_significant_value <= 127){
        return("INT1S")
      }
      if (rst_significant_value <= 32767){
        return("INT2S")
      }
      if (rst_significant_value <= 2147483647){
        return("INT4S")
      }
    }

    if (rst_signed == F){                             #INTU
      if (rst_significant_value <= 255){
        return("INT1U")
      }
      if (rst_significant_value <= 65534){
        return("INT2U")
      }
      if (rst_significant_value <= 4294967296){
        return("INT4U")
      }
    }
  }
}
