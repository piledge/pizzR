OTB_haralick <- function(filename_in=NULL, dir_out=NULL, xrad=3, yrad=3, nbbin=8, texture="advanced", channel=NULL, n_core=NULL, ram=NULL, otb_path = NULL){

  pizzR::package.install(c("memuse", "terra", "tools"), verbose = 1)
  in_file_cmd <- sprintf(' -in %s', filename_in)

  stopifnot(is.numeric(xrad) || is.numeric(yrad) || is.numeric(nbbin))
  xrad_cmd      <- sprintf(' -parameters.xrad %s', xrad)
  yrad_cmd      <- sprintf(' -parameters.yrad %s', yrad)
  nbbin_cmd     <- sprintf(' -parameters.nbbin %s', nbbin)

  n_bands <- terra::nlyr(terra::rast(filename_in))
  stopifnot(is.null(channel) || is.numeric(channel) && max(channel) <= n_bands)
  if (is.null(channel)) channel <- 1
  channel_cmd   <- sprintf(' -channel %s', channel)

  stopifnot(texture %in% c("simple","advanced","higher") && length(texture) == 1)
  texture_cmd  <- sprintf(' -texture %s', texture)

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

  if (is.null(dir_out))  dir_out <- sprintf('%s/haralick/', dirname(filename_in))
  pizzR::setcreate.wd(dir_out)
  out_file <- sub('.tif', sprintf("_%s_chan%s_xr%s_yr%s_nbbin%s.tif", texture, channel, xrad, yrad, nbbin), file.path(dir_out, basename(filename_in)))
  out_file_cmd  <- sprintf(' -out %s', out_file)

  pizzR::otb_setpath(otb_path)

  cmd <- sprintf('otbcli_HaralickTextureExtraction%s%s%s%s%s%s%s', in_file_cmd, out_file_cmd, xrad_cmd, yrad_cmd, nbbin_cmd, texture_cmd, ram_cmd)
  #cat(sprintf('\n%s\n', cmd))
  if (Sys.info()["sysname"] == "Windows") pizzR::OTB_run(cmd) else system(cmd)

}




OTB_haralick2 <- function(IMGpath=NULL,savedir=NULL,OTBpath=NULL,
                         band=NULL,texture=c("simple","advanced","higher"),
                         xrad=3,yrad=3,nbbin=8,
                         Ncore=parallel::detectCores()-1,ram=NULL, ...){
  warning('Deprecated. Please use OTB_haralick()')
  pizzR::package.install(c("memuse", "terra", "tools"), verbose = 1)

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
}

