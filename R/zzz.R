pronouncing <- NULL
pincelate <- NULL
pin <- NULL
.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  pronouncing <<- reticulate::import("pronouncing", delay_load = TRUE)
  pincelate <<- reticulate::import("pincelate", delay_load = TRUE)
  pin <<- pincelate$Pincelate()
}