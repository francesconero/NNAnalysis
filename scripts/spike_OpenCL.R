checkPlatform <- function(){
  if(length(oclPlatforms())<1){
    cat("No OpenCL platforms detected\n")
  } else {
    cat("Using platform:\n")
    oclInfo.clPlatformID(oclPlatforms()[[1]])
  }
}

checkDevices <- function(){
  if(length(oclDevices(platform = oclPlatforms()[[1]], type = "all"))<1){
    cat("No OpenCL devices on platform\n")
  } else {
    cat("Using device:\n")
    oclDevices()[[1]]
  }
}

loadKernel <- function(filename, functionName){
  cat("loading: ")
  cat(functionName)
  cat("\n")
  cat("from: ")
  cat(filename)
  cat("\n")
  code <- readChar(filename, file.info(filename)$size)
  return(oclSimpleKernel(device, functionName, code, "single"))
}

OpenCLSpikeDVCorr <- function(A, B, max_lag, wait_){
  return(oclRun(kernelSpikeDVCorr, max_lag,
                A, B,
                length(A), length(B),
                wait=wait_))
}

OpenCLSpikeDVCorrDefault <- function(A, B, max_lag=100, wait__=TRUE){
  return(OpenCLSpikeDVCorr(spikes[[A]],volts[[B]], max_lag, wait__))
}

setupOpencL <- function(){
  device <<- oclDevices()[[1]]
  cat("using device: ")
  cat(oclInfo.clDeviceID(device)$name)
  cat("\n")
  loadKernels()
}

OpenCLSpikeSpikeCorr <- function(trainA, trainB, max_lag, wait_=TRUE){
  return(oclRun(kernelSpikeSpikeCorr, max_lag,
                as.integer(trainA), as.integer(trainB),
                length(trainA), length(trainB),
                wait=wait_))
}

loadKernels <- function(){  
  kernelSpikeDVCorr <<- loadKernel("./scripts/spike_OpenCL.cpp", "spike_dv_corr")
  kernelConditional <<- loadKernel("./scripts/conditional_OpenCL.cpp", "test")
  kernelSpikeSpikeCorr <<- loadKernel("./scripts/spike_OpenCL.cpp", "spike_spike_corr")
}

cleanupOpenCL <- function(){
  rm(device, envir=globalenv())
  rm(kernelConditional, envir=globalenv())
  rm(kernelSpikeDVCorr, envir=globalenv())
  rm(kernelSpikeSpikeCorr, envir=globalenv())
}