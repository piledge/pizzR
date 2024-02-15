combine.semiparallel.extract <- function(ipath){
  ffiles <- list.files(ipath, pattern = '.feather', full.names = T)

  data <- vector('list', length(ffiles))
  for (i in seq(ffiles)) data[[i]] <- as.data.frame(feather::read_feather(ffiles[i]))

  extr <- pizzR::list.rbind(data)
  return(extr)
}
