.First <- function() {
  require(OpenCL)
  source("./scripts/spikes.R")
  source("./scripts/spike_OpenCL.R")  
  load(file="./.RData", envir=.GlobalEnv)
  cleanupOpenCL()
  setupOpencL()
  cat("\nSuccessfully loaded .Rprofile at", date(), "\n")
}

.Last <- function() {
  rm(list=lsf.str())
}
