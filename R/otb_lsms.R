OTB_lsms <- function(filename_in=NULL, dir_out=NULL, spatialr=5, ranger=15, minsize = 50, tilesizex=500, tilesizey = 500, mode="vector", n_core=NULL, ram=NULL, otb_path = NULL){

  pizzR::package.install(c("memuse", "tools"), verbose = 1)
  stopifnot(any(c('tif', 'tiff') %in% tools::file_ext(filename_in)))
  in_file_cmd <- sprintf(' -in %s', filename_in)

  stopifnot(is.numeric(spatialr) || is.numeric(ranger) || is.numeric(minsize) || is.numeric(tilesizex) || is.numeric(tilesizey))
  spatialr_cmd      <- sprintf(' -spatialr %s', spatialr)
  ranger_cmd      <- sprintf(' -ranger %s', ranger)
  minsize_cmd      <- sprintf(' -minsize %s', minsize)
  tilesizex_cmd     <- sprintf(' -tilesizex %s', tilesizex)
  tilesizey_cmd     <- sprintf(' -tilesizey %s', tilesizey)

  stopifnot(mode %in% c("vector","raster") && length(mode) == 1)
  mode_cmd  <- sprintf(' -mode.%s.out', mode)

  n_core_phys <- parallel::detectCores() - 1
  stopifnot(is.null(n_core) || is.numeric(n_core))
  if (is.null(n_core) || n_core >= n_core_phys) n_core <- parallel::detectCores() - 1
  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = n_core)

  stopifnot(is.null(ram) || is.numeric(ram))
  if (is.null(ram)) {
    avail.ram <- floor(memuse::Sys.meminfo()$totalram@size*1024)
    ram <- avail.ram - 3072
    if (ram < 4096) ram <- 4096
  }
  Sys.setenv(OTB_MAX_RAM_HINT = ram)
  ram_cmd <- sprintf(' -ram %s', ram)

  if (is.null(dir_out))  dir_out <- sprintf('%s/lsms/', dirname(filename_in))
  pizzR::setcreate.wd(dir_out)
  out_file <- sub('.tif', sprintf("_LSMS_sr%s_rr%s_ms%s_tx%s_ty%s.tif", spatialr, ranger, minsize, tilesizex, tilesizey), file.path(dir_out, basename(filename_in)))
  out_file_cmd  <- sprintf(' %s', out_file)
  out_file_cmd <- gsub('\\.', 'o', out_file_cmd)
  if (mode == 'vector') out_file_cmd <- gsub('otif', '.shp', out_file_cmd)
  if (mode == 'raster') out_file_cmd <- gsub('otif', '.tif', out_file_cmd)
  out_cmd <- sprintf('%s%s', mode_cmd, out_file_cmd)
  out_cmd

  pizzR::otb_setpath(otb_path)

  cmd <- sprintf('otbcli_LargeScaleMeanShift%s%s%s%s%s%s%s%s', in_file_cmd, out_cmd, spatialr_cmd, ranger_cmd, minsize_cmd, tilesizex_cmd, tilesizey_cmd, ram_cmd)
  cat(sprintf('\n%s\n', cmd))
  if (Sys.info()["sysname"] == "Windows") pizzR::OTB_run(cmd) else system(cmd)

}




OTB_lsms2 <- function(IMGpath=NULL,savedir=NULL,OTBpath=NULL,
                     spatrad=5,rangerad=15,minsize=100,
                     maxiter=100,thres=.001,fact=.5,
                     tilesizex=4096,tilesizey=4096,vectorize=TRUE,resume=TRUE,
                     Ncore=parallel::detectCores()-1,ram=NULL){
  warning('Deprecated. Please use OTB_haralick()')
  pizzR::package.install(c("memuse", "tools"), verbose = 1)

  if (is.null(ram)){
    avail.ram <- memuse::Sys.meminfo()$totalram@size*1024
    ram <- avail.ram - 3072
    if (ram < 4096) ram <- 4096
  }

  if (!any(c('tif', 'tiff') %in% tools::file_ext(IMGpath))) return(warning('IMGpath has to be a .tif-file.'))

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

  pizzR::OTB_init(path = OTBpath)

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
        if (domsfilt || !isTRUE(resume)) pizzR::OTB_run(cmd = filExe, Ncore = Ncore)

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
          if (domsseg || !isTRUE(resume)) pizzR::OTB_run(cmd = segExe, Ncore = Ncore)

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

            if (!file.exists(merOut) || !isTRUE(resume)) pizzR::OTB_run(cmd = merExe, Ncore = Ncore)

            if (isTRUE(vectorize)){
              vecnam <- gsub(pattern=".tif", replacement=".shp", merOut)
              vecExe <- paste("otbcli_LSMSVectorization",
                              "-in", IMGpath,
                              "-inseg", merOut,
                              "-out", vecnam,
                              "-tilesizex", tilesizex,
                              "-tilesizey", tilesizey, collapse=" ")

              if (!file.exists(vecnam) || !isTRUE(resume)){
                pizzR::OTB_run(cmd = vecExe, Ncore = Ncore)
                cat(sprintf('%s\n', vecExe))
              }
            }
          }
        }
      }
    }
  })
}
