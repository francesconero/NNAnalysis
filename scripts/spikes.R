spike_train <- function(timings, temporal_resolution){
  i = 1
  j = 0
  out = c()
  ordered_timings <- timings[order(timings)]
  while(j<=ordered_timings[i]){
    if((j<=ordered_timings[i]) & (ordered_timings[i]<=j+temporal_resolution)){
      out <- c(out, 1)
      if(i+1<length(ordered_timings)){
        i = i+1
      }
    } else {
      out <- c(out, 0)
    }
    j = j+temporal_resolution
  }
  return(out)
}

spike_train2 <- function(timings, temporal_resolution){
  normalized_timings <- timings/temporal_resolution
  normalized_timings <- floor(normalized_timings)
  out = rep.int(0, max(normalized_timings))
  for(timing in normalized_timings){
    out[timing] <- 1
  }
  return(out)
}

Find_Max_CCF<- function(ccfValues)
{
  cor = ccfValues$acf[,,1]
  lag = ccfValues$lag[,,1]
  res = data.frame(cor,lag,-cor/lag^3)
  res_max = res[which.max(res$cor),,]
  return(res_max)
} 

Find_Min_CCF<- function(ccfValues)
{
  cor = ccfValues$acf[,,1]
  lag = ccfValues$lag[,,1]
  res = data.frame(cor,lag,cor/lag^3)
  res_max = res[which.min(res$cor),,]
  return(res_max)
}

clean_detector <- function(detect){
  ids = unique(detect$V1)
  out <- list()
  for(id in ids){
    temp <- detect[ detect$V1==id, ]
    out[[as.character(id)]] <- temp[,-1]
  }
  return(out)
}

process_voltmeter <- function(voltmeter){
  out <- list()
  for(data in voltmeter){
    char <- as.character(data$V1[1])
    out[[char]] <- data[2:3]
  }
  return(out)
}

process_spike_detector <- function(spikeDetector,c=10){
  out = list()
  for(spikes in names(spikeDetector)){
    print(spikes)
    out[[spikes]] = as.integer(spikeDetector[[spikes]]*c)
  }
  return(out)
}

trainize <- function(spikeList, res=0.1){
  out <- list()
  for(spike in names(spikeList)){
    print(spike)
    out[[spike]] <- spike_train2(spikeList[[spike]],res)
  }
  return(out)
}

clean_past <- function(to_clean){
  out <- rev(to_clean$acf[ to_clean$lag < 0 ])
  return(out)
}

inferConnection <- function(ccg){
  if(max(ccg)>10000){
    cat(" possible excitatory connection\n")
  } else if(min(ccg)<(-5000)){
    cat(" possible inhibitory connection\n")
  } else {
    cat(" no connection\n")
  }
 out <- list()
 out[["max"]] <- max(ccg) 
 out[["min"]] <- min(ccg)
 out[["mean"]] <- mean(ccg)
 out[["values"]] <- ccg
 return(out)
}

examine_network_batched <- function(spikes, volts, max_lag){
  out <- NULL
  results <- NULL
  remaining <- spikes[-(1:2)]
  spikes <- spikes[1:2]
  print(length(remaining))
  while(length(remaining)>0){
    for(A in names(spikes)){
      for(B in names(volts)){
        if(A!=B){
          cat("launching ")
          cat(paste(A,B));
          cat("\n")
          results[[paste(A,B)]] <- opencl_corr(spikes[[A]],volts[[B]], max_lag, FALSE)
        }
      }
    }
    for(result in names(results)){
      cat("retrieving")
      cat(result)
      cat("\n")
      temp <- oclResult(results[[result]])
      while(is.null(temp))
        temp = oclResult(results[[result]])
      out <- rbind(out,cbind(data.frame(name = result, stringsAsFactors=FALSE), inferConnection(temp)))
    }
    results <- NULL
    spikes <- remaining[1:5]
    remaining <- remaining[-(1:5)]
  }
  return(out)
}

examine_network <- function(spikes, volts, max_lag){
  out <- NULL
  for(A in names(spikes)){
    for(B in names(volts)){
        print(paste(A,B));
        temp = OpenCLSpikeDVCorr(spikes[[A]],volts[[B]], max_lag, TRUE)
        out[[paste(A,B)]] <- temp
    }
  }
  return(out)
}

examine_network_default <- function(){
  return( examine_network(spikes, volts, 100))
}

setup_data <- function(raw_spikes, raw_volts){
  raw_spikes = clean_detector(raw_spikes)
  volts <<- clean_detector(raw_volts)
  spikes <<- process_spike_detector(raw_spikes)
}

computeScores <- function(network){
  out <- data.frame()
  for(name in names(network)){
    neurons <- strsplit(name, " ")[[1]]
    out <- rbind(out, data.frame(A=neurons[1], B=neurons[2], E = myE(network[[name]]), I = myI(network[[name]])))
  }
  return(out)
}

myI <- function(x)return(abs(min(diff(x))-sd(diff(x))))
myE <- function(x)return(abs(max(diff(x))-sd(diff(x))))

SpikeSpikeCorrDefault <- function(A, B, max_lag=100){
  return(SpikeSpikeCorr(spikes[[A]],spikes[[B]], max_lag=max_lag))
}

SpikeSpikeCorr <- function(A, B, max_lag=100, delay=0, plot=FALSE){
  deltaT <- min(B[length(B)], A[length(A)])
  out <- OpenCLSpikeSpikeCorr(A,B, max_lag=max_lag)/deltaT
  out[seq_len(delay)] = mean(out[-(seq_len(delay))])
  if(plot){
    plot(out, type='h')
    abline(h=AvgTrainRate(A)*AvgTrainRate(B), col="red")
    abline(h=mean(out), col="blue")
  }
  return(out)
}

SpikeDVCorr <- function(A, B, max_lag=100){
  return(OpenCLSpikeDVCorr(A,B, max_lag=max_lag, wait_=TRUE))
}

SpikeDVCorrDefault <- function(A, B, max_lag=100){
  return(SpikeDVCorr(spikes[[A]], spikes[[B]], max_lag=max_lag))
}

grubbs.flag <- function(x, sig.level=0.05) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv <= sig.level) {
    outliers <- c(outliers,grubbs.result$possible.outliers)
    test <- x[!x %in% outliers]
    if(length(test)==0){
      break
    }
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(x %in% outliers)
}

DetectMostLikelyLagBetweenTrainsDefault <- function(trainA, trainB){
  return(DetectMostLikelyLagBetweenTrains(spikes[[trainA]], spikes[[trainB]]))
}

DetectMostLikelyLagBetweenTrains <- function(trainA, trainB, sig.level=0.01, plot=FALSE, delay=0){
  corr <- SpikeSpikeCorr(trainA, trainB, delay=delay)
  outliers <- grubbs.flag(corr, sig.level=sig.level)
  if(plot){
    PlotOutliers(corr, outliers)
  }
  indexes <- which(outliers==TRUE)
  if(length(indexes) > 0){
    return(indexes-1)
  } else {
    return(-1)
  }
}

PlotOutliers <- function(data, outliers){
  plot(data, type='l')
  lines(which(outliers), data[outliers], type='p', col="red")
}

FilterTrainDefault <- function(trainToFilter, filterTrain, lag){
  return(FilterTrain(spikes[[trainToFilter]],
                     spikes[[filterTrain]],
                     lag=lag))
}

FilterTrain <- function(trainToFilter, filterTrain, lag){
  filterTrain = filterTrain - lag
  return(setdiff(trainToFilter, filterTrain))
}

PurgeTrain <- function(trainToFilter, filterTrain, sig.level=0.05, plot=FALSE, delay=0, forward){
  
  if(forward){
    parent = trainToFilter
    child = filterTrain
  } else {
    parent = filterTrain
    child = trainToFilter
  }
  
  lags <- DetectMostLikelyLagBetweenTrains(parent, child, sig.level=sig.level, delay=delay)
  out <- trainToFilter
  
  if(plot){
      plot(SpikeSpikeCorr(parent, child), type='l')
  }
  
  for(lag in lags){
    if(!forward){
      lag = -lag
    }
    out <- FilterTrain(out, filterTrain, lag)
  }
  
  if(plot){
    if(forward){
      lines(SpikeSpikeCorr(out, child), type='l', col="red")
    } else {
      lines(SpikeSpikeCorr(parent, out), type='l', col="red")
    }
  }
  
  cat("removed: ")
  cat(length(trainToFilter)-length(out))
  cat(" spikes (")
  cat(((length(trainToFilter)-length(out))/length(trainToFilter))*100)
  cat(" % of total)")
  return(out)
}

AvgTrainRate <- function(train){
  return(length(train)/train[length(train)])
}