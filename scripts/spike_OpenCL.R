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
  cat(filename)
  cat("\n")
  code <- readChar(filename, file.info(filename)$size)
  return(oclSimpleKernel(.device, functionName, code, "single"))
}

opencl_corr <- function(A, B, max_lag, wait_){
  return(oclRun(kernel, max_lag,
                A, B,
                length(A), length(B),
                wait=wait_))
}

opencl_corr_default <- function(A, B, max_lag=100, wait__=TRUE){
  return(opencl_corr(clean_spike_detector[[A]],clean_voltmeter[[B]], max_lag, wait__))
}

setupOpencL <- function(){
  .device <<- oclDevices()[[1]]
  cat("using device: ")
  cat(oclInfo.clDeviceID(.device)$name)
  cat("\n")
  loadKernels()
}

loadKernels <- function(){  
  .kernelSpike <<- loadKernel("./scripts/spike_OpenCL.cpp", "spike_correlogram")
  .kernelConditional <<- loadKernel("./scripts/conditional_OpenCL.cpp", "test")
}