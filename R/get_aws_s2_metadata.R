get_aws_s2_metadata <- function(metadata_path){
  doc <- xml2::read_xml(metadata_path)
  ns <- xml2::xml_ns(doc)
  
  geopos_node <- xml2::xml_find_first(doc, ".//Geoposition[@resolution='10']")
  doc_ns_stripped <- xml2::xml_ns_strip(doc)
  ulx_10 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./ULX", ns)))
  uly_10 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./ULY", ns)))
  xdim_10 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./XDIM", ns)))
  ydim_10 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./YDIM", ns)))
  res_param_10 <- c(xdim_10, abs(ydim_10))
  geopos_node <- xml2::xml_find_first(doc, ".//Size[@resolution='10']")
  doc_ns_stripped <- xml2::xml_ns_strip(doc)
  nrows_10 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./NROWS", ns)))
  ncols_10 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./NCOLS", ns)))
  ext_param_10 <- terra::ext(c(ulx_10, ulx_10 + (ncols_10 * abs(xdim_10)), uly_10 - (nrows_10 * abs(ydim_10)), uly_10))
  
  geopos_node <- xml2::xml_find_first(doc, ".//Geoposition[@resolution='20']")
  doc_ns_stripped <- xml2::xml_ns_strip(doc)
  ulx_20 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./ULX", ns)))
  uly_20 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./ULY", ns)))
  xdim_20 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./XDIM", ns)))
  ydim_20 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./YDIM", ns)))
  res_param_20 <- c(xdim_20, abs(ydim_20))
  geopos_node <- xml2::xml_find_first(doc, ".//Size[@resolution='20']")
  doc_ns_stripped <- xml2::xml_ns_strip(doc)
  nrows_20 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./NROWS", ns)))
  ncols_20 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./NCOLS", ns)))
  ext_param_20 <- terra::ext(c(ulx_20, ulx_20 + (ncols_20 * abs(xdim_20)), uly_20 - (nrows_20 * abs(ydim_20)), uly_20))
  
  geopos_node <- xml2::xml_find_first(doc, ".//Geoposition[@resolution='60']")
  doc_ns_stripped <- xml2::xml_ns_strip(doc)
  ulx_60 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./ULX", ns)))
  uly_60 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./ULY", ns)))
  xdim_60 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./XDIM", ns)))
  ydim_60 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./YDIM", ns)))
  res_param_60 <- c(xdim_60, abs(ydim_60))
  geopos_node <- xml2::xml_find_first(doc, ".//Size[@resolution='60']")
  doc_ns_stripped <- xml2::xml_ns_strip(doc)
  nrows_60 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./NROWS", ns)))
  ncols_60 <- as.numeric(xml2::xml_text(xml2::xml_find_first(geopos_node, "./NCOLS", ns)))
  ext_param_60 <- terra::ext(c(ulx_60, ulx_60 + (ncols_60 * abs(xdim_60)), uly_60 - (nrows_60 * abs(ydim_60)), uly_60))
  
  epsg <- xml2::xml_text(xml2::xml_find_first(doc_ns_stripped, ".//HORIZONTAL_CS_CODE"))
  epsg_numeric <- as.numeric(sub("EPSG:", "", epsg))
  crs_param <- pizzR::get.crsparams(epsg_numeric)
  
  lst <- list(
    EPSG=list(epsg=epsg_numeric, crs_param=crs_param),
    R10=list(ulx_10=ulx_10, uly_10=uly_10, xdim_10=xdim_10, ydim_10=ydim_10, res_param_10=res_param_10, nrows_10=nrows_10, nrows_10=ncols_10, ext_param_10=ext_param_10),
    R20=list(ulx_20=ulx_20, uly_20=uly_20, xdim_20=xdim_20, ydim_20=ydim_20, res_param_20=res_param_20, nrows_20=nrows_20, nrows_20=ncols_20, ext_param_20=ext_param_20),
    R60=list(ulx_60=ulx_60, uly_60=uly_60, xdim_60=xdim_60, ydim_60=ydim_60, res_param_60=res_param_60, nrows_60=nrows_60, nrows_60=ncols_60, ext_param_60=ext_param_60)
  )
  return(lst)
}