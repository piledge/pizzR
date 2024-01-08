OTB_lsms <- function(IMGpath=NULL,savedir=NULL,OTBpath=NULL,
                     spatrad=5,rangerad=15,minsize=100,
                     maxiter=100,thres=.001,fact=.5,
                     tilesizex=4096,tilesizey=4096,vectorize=TRUE,resume=TRUE,
                     Ncore=parallel::detectCores()-1,ram=1024){
  
  if (!any(c('tif', 'tiff') %in% tools::file_ext(IMGpath))) return(warning('IMGpath has to be a .tif-file.'))
  pizzR::setcreate.wd(savedir)
  
  
  
  OTB_init <- function(path = NULL){
    if(file.access(path) != 0){
      stop("Unable to access '", path, "'!")
    }
    options("OTB_PATH" = path)
    cat("'OTB_PATH' set to ", path)
  }
  
  OTB_run   <- function(cmd, Ncore = 1, DefaultRAM = NULL, ...){
    
    if (is.null(getOption("OTB_PATH"))){
      stop("OTB_PATH not found! Use 'OTB_init()' to set it...")
    }
    
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = Ncore)
    if (!is.null(DefaultRAM)) Sys.setenv(OTB_MAX_RAM_HINT = DefaultRAM)
    
    if (Sys.info()["sysname"] == "Windows"){
      
      # OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.cmd")
      OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.bat")
      stopifnot(file.access(OTB_PREFIX) == 0)
      OTB_PREFIX <- sprintf('"%s"', OTB_PREFIX)
    } else if (Sys.info()["sysname"] == "Linux"){
      
      OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.profile")
      stopifnot(file.access(OTB_PREFIX) == 0)
      # OTB_PREFIX <- sprintf('. "%s"', OTB_PREFIX)
      OTB_PREFIX <- sprintf('. %s', OTB_PREFIX)
      
    } else {
      OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.profile")
      OTB_PREFIX <- system(sprintf('echo %s', OTB_PREFIX), intern = TRUE)
      stopifnot(file.access(OTB_PREFIX) == 0)
      OTB_PREFIX <- sprintf('. "%s"', OTB_PREFIX)
    }
    
    try({
      cmd <- sprintf('%s && %s', OTB_PREFIX, cmd)
      cat("\nRunning:\n", cmd, "\n\n")
      system(cmd, ...)
    })
  }
  
  basenam <- gsub(pattern = "[.][[:print:]]*$", replacement = "", IMGpath)
  
  if (is.null(savedir)) savedir <- paste0(basenam, "_LSMS")
  if (file.access(savedir) < 0) dir.create(savedir, recursive = TRUE)
  
  if (is.null(dim(fact))){
    sr_fact  <- rr_fact <- fact
    uni_fact <- TRUE
  } else if (ncol(fact) == 2){
    sr_fact  <- fact[, 1]
    rr_fact  <- fact[, 2]
    uni_fact <- FALSE
  }
  
  basenam <- file.path(savedir, basename(basenam))
  exten   <- ".tif"
  
  OTB_init(path = OTBpath)
  
  try(expr = {
    
    for (i in seq_along(spatrad)){
      for (ii in seq_along(rangerad)){
        
        sr1 <- formatC(spatrad[i], decimal.mark = "o")
        rr1 <- formatC(rangerad[ii], decimal.mark = "o")
        
        filtsnam <- sprintf("%s_FILT_sr%s_rr%s_LSMS_filts%s", 
                            basenam, sr1, rr1, exten)
        filtrnam <- sprintf("%s_FILT_sr%s_rr%s_LSMS_filtr%s", 
                            basenam, sr1, rr1, exten)
        
        filExe <- paste("otbcli_MeanShiftSmoothing",
                        "-in", IMGpath,
                        "-fout", filtrnam,
                        "-foutpos", filtsnam,
                        "-spatialr", spatrad[i],
                        "-ranger", rangerad[ii],
                        "-maxiter", maxiter,
                        "-thres", thres,
                        "-ram", ram,
                        "-modesearch", 0, collapse=" ")
        
        domsfilt <- !(file.exists(filtrnam) && file.exists(filtsnam))
        if (domsfilt || !isTRUE(resume)) OTB_run(cmd = filExe, Ncore = Ncore)
        
        for (iii in seq_along(rr_fact)){
          
          
          if (isTRUE(uni_fact)){
            segm0nam <- sprintf("%s_FILT_sr%s_rr%s_SEGM_f%s_LSMS_segm%s", 
                                basenam, sr1, rr1, 
                                formatC(fact[iii], decimal.mark = "o"), exten)
          } else {
            segm0nam <- sprintf("%s_FILT_sr%sX%s_rr%sX%s_SEGM_LSMS_segm%s", 
                                basenam, sr1, gsub("\\.", "o", round(sr_fact[iii], 3)),
                                rr1, gsub("\\.", "o", round(rr_fact[iii], 3)), exten)
          }
          
          
          segExe <- paste("otbcli_LSMSSegmentation",
                          "-in", filtrnam,
                          "-inpos", filtsnam,
                          "-out", segm0nam, "uint32",
                          "-spatialr", spatrad[i] * sr_fact[iii],
                          "-ranger", rangerad[ii] * rr_fact[iii],
                          "-minsize", 0,
                          "-tmpdir", tempdir(),
                          "-tilesizex", tilesizex,
                          "-tilesizey", tilesizey, collapse=" ")
          
          
          domsseg  <- !file.exists(segm0nam)
          if (domsseg || !isTRUE(resume)) OTB_run(cmd = segExe, Ncore = Ncore)
          
          for (iv in seq_along(minsize)){
            
            merOut <- gsub(sprintf("_LSMS_segm%s", exten),
                           sprintf("_ms%s_LSMS_segm%s", minsize[iv] , exten), 
                           segm0nam)
            
            merExe <- paste("otbcli_LSMSSmallRegionsMerging",
                            "-in", filtrnam,
                            "-inseg", segm0nam,
                            "-out", merOut, "uint32",
                            "-minsize", minsize[iv],
                            "-tilesizex", tilesizex,
                            "-tilesizey", tilesizey, collapse=" ")
            
            if (!file.exists(merOut) || !isTRUE(resume)) OTB_run(cmd = merExe, Ncore = Ncore)
            
            if (isTRUE(vectorize)){
              vecnam <- gsub(pattern=".tif", replacement=".shp", merOut)
              vecExe <- paste("otbcli_LSMSVectorization",
                              "-in", IMGpath,
                              "-inseg", merOut,
                              "-out", vecnam,
                              "-tilesizex", tilesizex,
                              "-tilesizey", tilesizey, collapse=" ")
              
              if (!file.exists(vecnam) || !isTRUE(resume)) OTB_run(cmd = vecExe, Ncore = Ncore)
              
            }
          }
        }
      }
    }
  })
}
