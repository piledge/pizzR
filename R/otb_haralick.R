OTB_haralick <- function(IMGpath=NULL,savedir=NULL,OTBpath=NULL,
                         band=NULL,texture=c("simple","advanced","higher"),
                         xrad=3,yrad=3,nbbin=8,
                         Ncore=parallel::detectCores()-1,ram=NULL, ...){

  pizzR::package.install(c("memuse", "tools"), verbose = 1)

  if (is.null(ram)){
    avail.ram <- memuse::Sys.meminfo()$totalram@size*1024
    ram <- avail.ram - 3072
    if (ram < 4096) ram <- 4096
  }

  if (!any(c('tif', 'tiff') %in% tools::file_ext(IMGpath))) return(warning('IMGpath has to be a .tif-file.'))

  basenam <- gsub(pattern = "[.][[:print:]]*$", replacement = "", IMGpath)
  if (is.null(savedir)) savedir <- paste0(basenam, "_LSMS")
  if (is.null(band)) band <- seq_len(terra::nlyr(terra::rast(IMGpath)))

  pizzR::setcreate.wd(savedir)
  for (i in seq_along(band)){

    texture <- match.arg(arg = texture, choices =  c("simple", "advanced", "higher"))
    pizzR::OTB_init(path = OTBpath)



    filename <- file.path(savedir, basename(IMGpath))
    filename <- gsub(pattern = pizzR::extension(filename),
                     replacement = sprintf("_b%s_%s_xr%s_yr%s_nbbin%s.tif",
                                           band[i], texture, xrad, yrad, nbbin),
                     x = filename)

    cmd1 <- sprintf("otbcli_HaralickTextureExtraction -in %s -channel %s -parameters.xrad %s -parameters.yrad %s -parameters.nbbin %s -texture %s -ram %s -out %s",
                    IMGpath, band[i], xrad, yrad, nbbin, texture, ram, filename)
    print(cmd1)
    if (!file.exists(filename)) pizzR::OTB_run(cmd1, ...)
  }
  cat(paste0("\n", pizzR::Systime(),": Files written to '", savedir, "'!"))
}

