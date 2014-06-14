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
    temp <- detect[ detect$V1==id, ,drop=FALSE]
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

process_spike_detector <- function(spikeDetector){
  out = list()
  for(spikes in names(spikeDetector)){
    print(spikes)
    out[[spikes]] = as.integer(spikeDetector[[spikes]]*10)
  }
  return(out)
}

trainize <- function(spikeList){
  out <- list()
  for(spike in names(spikeList)){
    print(spike)
    out[[spike]] <- spike_train2(spikeList[[spike]],0.1)
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
      if(A!=B){
        cat(paste(A,B));
        temp = opencl_corr(spikes[[A]],volts[[B]], max_lag, TRUE)
        temp = inferConnection(temp)
        temp[["num_spikes"]] <- length(spikes[[A]])
        out[[paste(A,B)]] <- append(out,temp)
      }
    }
  }
  return(out)
}

examine_network_default <- function(){
  return( examine_network(spikes=clean_spike_detector, volts=clean_voltmeter, 100))
}

neg <- function(x)-x