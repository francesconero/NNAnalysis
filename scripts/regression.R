LinearRegressionFit <- function(Y, X, window.size, auto.regression.size, intercept=TRUE, batch.size=40000){
  next.cut <- ifelse(batch.size<length(Y), batch.size, length(Y))
  next.batch <- data.frame(Y=Y[seq_len(next.cut)],X=X[seq_len(next.cut),])
  Y <- Y[-seq_len(next.cut)]
  X <- X[-seq_len(next.cut),]
  if(intercept)
    fit <- speedlm(Y~., next.batch)
  else
    fit <- speedlm(Y~0+., next.batch)
  attr(fit$terms, ".Environment") <- NULL
  while(length(Y)>0){
    next.cut <- ifelse(batch.size<length(Y),batch.size,length(Y))
    next.batch <- data.frame(Y=Y[seq_len(next.cut)],X=X[seq_len(next.cut),])
    Y <- Y[-seq_len(next.cut)]
    X <- X[-seq_len(next.cut),]
    fit <- update(fit, next.batch)
    attr(fit$terms, ".Environment") <- NULL
  }
  
  fit[['window.size']] <- window.size
  fit[['autoregression.size']] <- auto.regression.size
  attr(fit$terms, ".Environment") <- NULL
  return(fit)
}

LinearRegressionFit.GLM <- function(Y, X, window.size, auto.regression.size, intercept=TRUE, batch.size=40000){
  next.cut <- ifelse(batch.size<length(Y),batch.size,length(Y))
  next.batch <- data.frame(Y=Y[seq_len(next.cut)],X=X[seq_len(next.cut),])
  Y <- Y[-seq_len(next.cut)]
  X <- X[-seq_len(next.cut),]
  if(intercept)
    fit <- speedglm(Y~., next.batch, family=binomial(), maxit = 100)
  else
    fit <- speedglm(Y~0+., next.batch, family=binomial(), maixt = 100)
  attr(fit$terms, ".Environment") <- NULL
  while(length(Y)>0){
    next.cut <- ifelse(batch.size<length(Y),batch.size,length(Y))
    next.batch <- data.frame(Y=Y[seq_len(next.cut)],X=X[seq_len(next.cut),])
    Y <- Y[-seq_len(next.cut)]
    X <- X[-seq_len(next.cut),]
    fit <- update(fit, next.batch)
    attr(fit$terms, ".Environment") <- NULL
  }
  
  fit[['window.size']] <- window.size
  fit[['autoregression.size']] <- auto.regression.size
  attr(fit$terms, ".Environment") <- NULL
  return(fit)
}

GetChunkOfSpikes <- function(target, start, chunk.size, window.size, except=c()){
  i <- start
  out = matrix(ncol=(length(trainized.spikes[setdiff(nodes$name,except)]))*window.size,nrow=chunk.size)
  p = 1
  for(neuron in trainized.spikes[setdiff(nodes$name,except)]){
    for(j in seq_len(window.size)){
      out[,p] = neuron[(i-window.size+j):(i-window.size+j+chunk.size-1)]
      p = p+1
    }
  }
  return(out)
}

GetChunkOfSpikes.GLM <- function(target, start, chunk.size, window.size, spike.cluster.count, except=c()){
  i <- start
  W <- window.size
  M <- spikes.cluster.count
  out = matrix(ncol=(length(trainized.spikes[setdiff(nodes$name,except)]))*M,nrow=chunk.size)
  p = 1
  for(neuron in trainized.spikes[setdiff(nodes$name,except)]){
    for(m in seq_len(M)){
      cluster <- matrix(ncol=W,nrow=nrow(out))
      m.count = 1
      for(j in seq_len(W)){
        cluster[,m.count] <- neuron[(i-(m-1)*W+j):(i-(m-1)*W+j+chunk.size-1)]
        m.count = m.count+1
      }
      out[,p] <- rowSums(cluster)
      p = p+1
    }
  }
  return(out)
}

GetChunkOfHistory <- function(signal, start, chunk.size, window.size){
  out = matrix(ncol=window.size,nrow=chunk.size)
  p = 1
  for(j in seq_len(window.size)){
    out[,p] = signal[(start-window.size+j):(start-window.size+j+chunk.size-1)]
    p = p+1
  }
  return(out)
}

ExamineNeuron <- function(neuron){
  values <- dVolts[[neuron]]
  to.predict <- values[10001:50000]
  own.history <- GetChunkOfHistory(values, start=10000, chunk.size=40000, window.size=100)
  print("Calculating total deviance...")
  predictor.spikes <- GetChunkOfSpikes(neuron, 10000, 40000, 30, c())
  predictor.spikes <- cbind(own.history, predictor.spikes)
  fits <- list()
  fits[['total']] <- LinearRegressionFit(to.predict,
                                         predictor.spikes,
                                         window.size=10,
                                         auto.regression.size=100,
                                         intercept=FALSE
  )
  for(excluded.neuron in nodes$name){
    print(paste("Deviance without neuron ", excluded.neuron))
    predictor.spikes <- GetChunkOfSpikes(neuron, 10000, 40000, 30, c(excluded.neuron))
    predictor.spikes <- cbind(own.history, predictor.spikes)
    fits[[excluded.neuron]] <- LinearRegressionFit(to.predict,
                                                   predictor.spikes,
                                                   window.size=10,
                                                   auto.regression.size=100,
                                                   intercept=FALSE
    )
    fits[[excluded.neuron]][['excluded.neuron']] <- excluded.neuron
  }
  return(fits)
}

ExamineNeuron.GLM <- function(neuron){
  values <- trainized.spikes[[neuron]]
  to.predict <- values[10001:50000]
  print("Calculating total deviance...")
  predictor.spikes <- GetChunkOfSpikes.GLM(neuron, 10000, 40000, 20, 3, c())
  fits <- list()
  fits[['total']] <- LinearRegressionFit.GLM(to.predict,
                                             predictor.spikes,
                                             window.size=3,
                                             auto.regression.size=0,
                                             intercept=TRUE
  )
  for(excluded.neuron in nodes$name){
    print(paste("Deviance without neuron ", excluded.neuron))
    predictor.spikes <- GetChunkOfSpikes.GLM(neuron, 10000, 40000, 20, 3, c(excluded.neuron))
    fits[[excluded.neuron]] <- LinearRegressionFit.GLM(to.predict,
                                                       predictor.spikes,
                                                       window.size=3,
                                                       auto.regression.size=0,
                                                       intercept=TRUE
    )
    fits[[excluded.neuron]][['excluded.neuron']] <- excluded.neuron
  }
  return(fits)
}

ExamineNetwork <- function(){
  out <- list()
  for(neuron in nodes[!nodes$is.input,]$name){
    print(paste("Examining  neuron: ", neuron))
    out[[neuron]] <- ExamineNeuron(neuron)
  }
  return(out)
}

ExamineNetwork.GLM <- function(){
  out <- list()
  for(neuron in nodes[!nodes$is.input,]$name){
    print(paste("Examining  neuron: ", neuron))
    out[[neuron]] <- ExamineNeuron.GLM(neuron)
  }
  return(out)
}

PredictSpeedlm <- function(fit, new.data){
  intercept = attr(fit$terms, 'intercept')==1
  if(intercept){
    coefs=coef(fit)[-1]
  }else{
    coefs=coef(fit)
  }
  out = new.data%*%coefs
  if(intercept){
    out = out+coef(fit)[1]
  }
  return(as.vector(out))
}

ResidualsSpeedlm <- function(fit, values, new.data){
  predicted = PredictSpeedlm(fit, new.data)
  return(values-predicted)
}