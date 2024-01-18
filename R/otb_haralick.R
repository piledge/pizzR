OTB_Haralick <- function(x, band = NULL, savedir, texture = c("simple", "advanced", "higher"), xrad = 3, yrad = 3,
                         ram=4096, nbbin = 8, OTBpath = "C:/OTB-8.1.2-Win64", ...){

  if (is.null(band)) band <- seq_len(terra::nlyr(terra::rast(x)))

  for (i in seq_along(band)){

    texture <- match.arg(arg = texture, choices =  c("simple", "advanced", "higher"))
    pizzR::OTB_init(path = OTBpath)

    pizzR::setcreate.wd(savedir)

    filename <- file.path(savedir, basename(x))
    filename <- gsub(pattern = pizzR::extension(filename),
                     replacement = sprintf("_b%s_%s_xr%s_yr%s_nbbin%s.tif",
                                           band[i], texture, xrad, yrad, nbbin),
                     x = filename)

    cmd1 <- sprintf("otbcli_HaralickTextureExtraction -in %s -channel %s -parameters.xrad %s -parameters.yrad %s -parameters.nbbin %s -texture %s -ram %s -out %s",
                    x, band[i], xrad, yrad, nbbin, texture, ram, filename)
    print(cmd1)
    if (!file.exists(filename)) pizzR::OTB_run(cmd1, ...)
  }
}
