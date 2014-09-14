GenerateAdjacencyMatrix <- function(){
  rnames = nodes$name
  cnames = nodes[!nodes$is.input,]$name
  adjacency.matrix <<- list()
  adjacency.matrix[['connections']] <<- matrix(data=0,
                                               nrow=nrow(nodes),
                                               ncol=nrow(nodes[!nodes$is.input,]),
                                               dimnames=list(rnames,cnames)
  )
  adjacency.matrix[['delays']] <<- matrix(data=0,
                                          nrow=nrow(nodes),
                                          ncol=nrow(nodes[!nodes$is.input,]),
                                          dimnames=list(rnames,cnames)
  )
  adjacency.matrix[['connections.complete']] <<- matrix(data=0,
                                                        nrow=nrow(nodes),
                                                        ncol=nrow(nodes),
                                                        dimnames=list(rnames,rnames)
  )
  adjacency.matrix[['delays.complete']] <<- matrix(data=0,
                                                   nrow=nrow(nodes),
                                                   ncol=nrow(nodes),
                                                   dimnames=list(rnames,rnames)
  )
  for(i in seq_len(nrow(connections))){
    action.type = nodes[nodes$name==connections[i,]$node.A,]$type
    adjacency.matrix[['connections']][connections[i,]$node.A, connections[i,]$node.B] <<- ifelse(action.type=='E',1,-1)
    adjacency.matrix[['delays']][connections[i,]$node.A, connections[i,]$node.B] <<- connections[i,]$delay
    adjacency.matrix[['connections.complete']][connections[i,]$node.A, connections[i,]$node.B] <<- ifelse(action.type=='E',1,-1)
    adjacency.matrix[['delays.complete']][connections[i,]$node.A, connections[i,]$node.B] <<- connections[i,]$delay
  }
  adjacency.matrix[['logical']] <<- adjacency.matrix[['connections']]!=0
}

RenameIDs <- function(){
  spikes.raw$name <<- nodes$name[match(spikes.raw$ID,nodes$ID)]
  spikes.raw$ID <<- NULL
  volts.raw$name <<- nodes$name[match(volts.raw$ID,nodes$ID)]
  volts.raw$ID <<- NULL
}

UpdateData <- function(settings){
  spikes <<- CleanSpikes(spikes.raw)
  volts <<- CleanVolts(volts.raw)
  trainized.spikes <<- lapply(spikes, SpikeTrain, settings$resolution, settings$simulation.time)
  dVolts <<- lapply(volts, gradient)
  dVolts.processed <<- lapply(dVolts, sign)
}

CleanSpikes <- function(spikes){
  out = split(spikes$value, spikes$name)
  ordered <- order(nodes$name)
  back <- order(ordered)
  out = out[order(nodes$name)]
  for(name in setdiff(nodes$name,names(out))){
    out[[name]]<-vector('numeric')
  }
  out <- out[order(names(out))]
  out <- out[back]
  return(out)
}

CleanVolts <- function(volts){
  out = split(volts$value, volts$name)
  out = out[order(names(out))]
  return(out)
}

SpikeTrain <- function(timings, temporal.resolution, duration){
  normalized_timings <- timings/temporal.resolution
  normalized_timings <- floor(normalized_timings)
  out = rep.int(0, duration/temporal.resolution)
  for(timing in normalized_timings){
    out[timing] <- 1
  }
  return(out)
}

PadMatrix <- function(m, pad){
  rnames <- sort(nodes$name)
  out <- matrix(data=pad,
                nrow=nrow(nodes),
                ncol=nrow(nodes),
                dimnames=list(rnames,rnames)
  )
  out[seq_len(nrow(m)),seq_len(ncol(m))] <- m 
  return(out)
}
