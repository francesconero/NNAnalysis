.First <- function() {
  require(pracma)
  require(speedglm)
  require(lattice)
  require(doParallel)
  require(forecast)
  load("./.RData", .GlobalEnv)
  source("./scripts/regression.R")
  source("./scripts/manager.R")
  source("./scripts/processor.R")
  source("./scripts/draw.R")
  source("./scripts/analysis.R")
  cat("\nSuccessfully loaded .Rprofile at", date(), "\n")
}

.Last <- function(){
  rm(list = lsf.str())
}