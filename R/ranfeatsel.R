ranFeatsel <- function (data, classes, ntree = 1000, nthreads = parallel::detectCores() - 1,
                        savename = "ranFeatsel", savedir = getwd(), keep.files = FALSE,
                        best_thr = 0.975, nimpplot = 20, seed = NULL, ...)
{
  pizzR::package.install(c("crayon", "parallel", "ranger", "vip"), verbose = 1)

  if (is.null(seed)){
    seed <- sample(seq(1000000000), 1, replace=TRUE)
    cat(crayon::red(paste0("\n ", pizzR::Systime(), ': Using random seed of ', seed, ". Specify 'seed' if required static.", "\n")))
  }
  set.seed(seed)

  st.featsel <- as.integer(format(Sys.time(), "%s"))
  cat("\n")
  cat("               \r", paste0(pizzR::Systime(), ": starting recursive MDA-feature selection",
                                  "\n"))

  pizzR::setcreate.wd(savedir)

  if ((nthreads > parallel::detectCores() - 1)) {
    nthreads <- parallel::detectCores() - 1
  }
  dots <- list(...)
  dots$x <- data[, -grep(classes, colnames(data))]
  dots$y <- as.factor(data[, grep(classes, colnames(data))])
  dots$num.trees <- ntree
  dots$num.threads <- nthreads
  dots$importance <- "permutation"
  dots$keep.inbag <- TRUE
  dots$classification <- TRUE
  N <- max(nchar(ncol(dots$x)))
  rf_type <- "MDA"
  for (i in 1:(ncol(data) - 1)) {
    if (i == 1) {
      cat("               \r", paste0(pizzR::Systime(), ": remaining loops: ",
                                      ncol(data) - (i)))
    }
    else {
      dur <- round((as.integer(format(Sys.time(), "%s")) -
                      st)/60, 2)
      cat("               \r", paste0(pizzR::Systime(), ": remaining loops: ",
                                      ncol(data) - (i), "   |   duration last loop: ",
                                      dur, " minutes        "))
    }
    st <- as.integer(format(Sys.time(), "%s"))
    ranger_submod <- do.call(ranger::ranger, dots)
    nvariables <- ncol(dots$x)
    least.importance <- names(sort(ranger_submod$variable.importance,
                                   decreasing = FALSE))[1]
    dots$x <- dots$x[colnames(dots$x) != least.importance]

    cm <- pizzR::confMatrix((ranger_submod[["predictions"]]), ranger_submod[["call"]][["y"]])
    OA <- cm[["OA"]]
    kappa <- cm[["KAPPA"]]

    if (i == 1) {
      OOB_OA <- data.frame(cbind(i, OA, kappa, least.importance,
                                 nvariables))
    }
    else {
      OOB_OA <- rbind(OOB_OA, cbind(i, OA, kappa, least.importance,
                                    nvariables))
    }
    if (i == ncol(data) - 1) {
      colnames(OOB_OA) <- c("loopID", "oobOA", "kappa",
                            "least_imp", "nvariables")
      OOB_OA$loopID <- as.numeric(OOB_OA$loopID)
      OOB_OA$oobOA <- as.numeric(OOB_OA$oobOA)
      OOB_OA$kappa <- as.numeric(OOB_OA$kappa)
      OOB_OA$nvariables <- as.numeric(OOB_OA$nvariables)
    }
    save(ranger_submod, file = sprintf(paste0("%s/%s_fs_%s_classif_%0",
                                              N, "d.Rdata"), savedir, savename, rf_type, (ncol(data)) -
                                         (i)))
    if (i == (ncol(data) - 1)) {
      dur <- round((as.integer(format(Sys.time(), "%s")) -
                      st)/60, 2)
      cat("               \r", paste0(pizzR::Systime(), ": remaining loops: 0   |    duration last loop: ",
                                      dur, " minutes                      "))
    }
  }
  best_qu <- quantile(OOB_OA$oobOA, probs = best_thr)
  fittest.models <- subset(OOB_OA, OOB_OA$oobOA >= best_qu)
  fittest.model <- subset(fittest.models, fittest.models$nvariables ==
                            min(fittest.models$nvariables))
  file.copy(sprintf(paste0("%s/%s_fs_%s_classif_%0", N, "d.Rdata"),
                    savedir, savename, rf_type, as.integer(fittest.model$nvariables)),
            sprintf(paste0("%s/%s_fs_%s_classif_", "fittest.Rdata"),
                    savedir, savename, rf_type), overwrite = TRUE)
  best.model <- subset(subset(OOB_OA, OOB_OA$oobOA == max(OOB_OA$oobOA)),
                       subset(OOB_OA, OOB_OA$oobOA == max(OOB_OA$oobOA))$nvariables ==
                         min(subset(OOB_OA, OOB_OA$oobOA == max(OOB_OA$oobOA))$nvariables))
  file.copy(sprintf(paste0("%s/%s_fs_%s_classif_%0", N, "d.Rdata"),
                    savedir, savename, rf_type, as.integer(best.model$nvariables)),
            sprintf(paste0("%s/%s_fs_%s_classif_", "best.Rdata"),
                    savedir, savename, rf_type), overwrite = TRUE)


  load(sprintf(paste0("%s/%s_fs_%s_classif_", "fittest.Rdata"),
               savedir, savename, rf_type))
  fittest.ranger <- ranger_submod

  cm.fittest.all <- pizzR::confMatrix((fittest.ranger[["predictions"]]), fittest.ranger[["call"]][["y"]])
  cm.fittest <- cm.fittest.all$CM
  acc.fittest <- cm.fittest.all$OA
  kappa.fittest <- cm.fittest.all$KAPPA

  load(sprintf(paste0("%s/%s_fs_%s_classif_", "best.Rdata"),
               savedir, savename, rf_type))
  best.ranger <- ranger_submod

  cm.best.all <- pizzR::confMatrix((best.ranger[["predictions"]]), best.ranger[["call"]][["y"]])
  cm.best <- cm.best.all$CM
  acc.best <- cm.best.all$OA
  kappa.best <- cm.best.all$KAPPA


  write.csv2(cm.fittest, sprintf(paste0("%s/%s_fs_%s_classif_",
                                        "fittest_cm.csv"), savedir, savename, rf_type))
  write.csv2(fittest.model, sprintf(paste0("%s/%s_fs_%s_classif_",
                                           "fittest_metrics.csv"), savedir, savename, rf_type))
  write.csv2(cm.best, sprintf(paste0("%s/%s_fs_%s_classif_",
                                     "best_cm.csv"), savedir, savename, rf_type))
  write.csv2(best.model, sprintf(paste0("%s/%s_fs_%s_classif_",
                                        "best_metrics.csv"), savedir, savename, rf_type))
  if (keep.files == F) {
    cat("               \n", paste0(pizzR::Systime(), ": removing submodels   |                                                                 "))
    file.remove(list.files(savedir, pattern = ".Rdata", full.names = TRUE)[-c(grep("fittest",
                                                                                   list.files(savedir, pattern = ".Rdata", full.names = TRUE)),
                                                                              grep("best", list.files(savedir, pattern = ".Rdata",
                                                                                                      full.names = TRUE)))])
  }
  plot(OOB_OA$oobOA ~ OOB_OA$nvariables, type = "l", main = "recursive MDA-feature selection",
       xlab = "nVariables", ylab = "OOB-OA", col = "red3", lwd = "2")
  if (fittest.model$loopID != best.model$loopID) {
    abline(v = fittest.model$nvariables, col = "blue3", lwd = 1,
           lty = 2)
    abline(v = best.model$nvariables, col = "green3", lwd = 1,
           lty = 2)
    abline(h = fittest.model$oobOA, col = "blue3", lwd = 1,
           lty = 2)
    abline(h = best.model$oobOA, col = "green3", lwd = 1,
           lty = 2)
    legend("bottomright", legend = c("fittest model", "best model"),
           col = c("blue3", "green3"), lwd = 1, lty = 2, cex = 0.8)
  }
  else {
    abline(v = fittest.model$nvariables, col = "blue3", lwd = 1,
           lty = 2)
    abline(h = fittest.model$oobOA, col = "blue3", lwd = 1,
           lty = 2)
    legend("bottomright", legend = c("fittest/best model"),
           col = c("blue3"), lwd = 1, lty = 2, cex = 0.8)
  }
  if (length(fittest.ranger[["variable.importance"]]) > nimpplot) {
    plot.varimp.fittest <- nimpplot
  }
  else {
    plot.varimp.fittest <- length(fittest.ranger[["variable.importance"]])
  }
  fittest.varimpplot <- vip::vip(fittest.ranger, num_features = plot.varimp.fittest,
                                 horizontal = T, geom = "point", aesthetics = list(size = 2))
  if (length(best.ranger[["variable.importance"]]) > nimpplot) {
    plot.varimp.best <- nimpplot
  }
  else {
    plot.varimp.best <- length(best.ranger[["variable.importance"]])
  }
  best.varimpplot <- vip::vip(best.ranger, num_features = plot.varimp.best,
                              horizontal = T, geom = "point", aesthetics = list(size = 2))
  dur.featsel <- round((as.integer(format(Sys.time(), "%s")) -
                          st.featsel)/60, 2)
  cat("               \n", paste0(pizzR::Systime(), ": run completed",
                                  "        |    duration of run:     ", dur.featsel, " minutes             \n\n\n"))
  cat(paste0("fittest model: OOB-OA = ", round(fittest.model$oobOA, 3),
             "%; Kappa = ", round(fittest.model$kappa, 3), "; nVariables = ",
             fittest.model$nvariables, "\n"))
  print(cm.fittest)
  cat(paste0("\n\nbest model:    OOB-OA = ", round(best.model$oobOA, 3),
             "%; Kappa = ", round(best.model$kappa, 3), "; nVariables = ", best.model$nvariables,
             "\n"))
  print(cm.best)
  if (fittest.model$loopID == best.model$loopID) {
    cat(crayon::red("\nNOTE: fittest model and best model are equal\n"))
  }
  return(list(OOB_OA = OOB_OA,
              ranFeatsel.fittest = fittest.ranger, ranFeatsel.best = best.ranger,
              cm.fittest = cm.fittest.all, cm.best = cm.best.all,
              fittest.model.metrics = fittest.model, best.model.metrics = best.model,
              fittest.varimpplot = fittest.varimpplot, best.varimpplot = best.varimpplot,
              fittest.features = names(fittest.ranger[["variable.importance"]]), best.features = names(best.ranger[["variable.importance"]]),
              used.seed = seed)
  )
}
