OTB_lsms <- function(IMGpath=NULL,savedir=NULL,OTBpath=NULL,
                     spatrad=5,rangerad=15,minsize=100,
                     maxiter=100,thres=.001,fact=.5,
                     tilesizex=4096,tilesizey=4096,vectorize=TRUE,resume=TRUE,
                     Ncore=parallel::detectCores()-1,ram=NULL){

  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)) {
      cat(paste0(pizzR::Systime(), ": install missing packages '", paste(x[to_install], collapse = ", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(pizzR::Systime(), ": missing packages '", paste(x[to_install], collapse = ", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "tools"))

  if (!any(c('tif', 'tiff') %in% tools::file_ext(IMGpath))) return(warning('IMGpath has to be a .tif-file.'))
    if (is.null(ram)){
    avail.ram <- memuse::Sys.meminfo()$totalram@size*1024
    ram <- avail.ram - 3072
    if (ram < 4096) ram <- 4096
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

              if (!file.exists(vecnam) || !isTRUE(resume)) pizzR::OTB_run(cmd = vecExe, Ncore = Ncore)

            }
          }
        }
      }
    }
  })
  cat(paste0("\n", pizzR::Systime(),": Files written to '", savedir, "'!"))
}
