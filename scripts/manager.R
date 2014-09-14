LoadData <- function(path, ID){
  prefix = paste(path,ID,sep="")
  
  nodes.filename = paste(prefix,".nod",sep="")
  connection.filename = paste(prefix,".con",sep="")
  spikes.filename = paste(prefix,".spi",sep="")
  volts.filename = paste(prefix,".vol",sep="")
  settings.filename = paste(prefix, ".set",sep="")
  
  nodes <<- read.delim(nodes.filename, colClasses=c('character','integer','logical','character'))
  not.input.nodes <- nodes[!nodes$is.input,]
  input.nodes <- nodes[nodes$is.input,]
  nodes <<- rbind(not.input.nodes[order(not.input.nodes$name),],input.nodes[order(input.nodes$name),])
  row.names(nodes) <<- NULL
  connections <<- read.delim(connection.filename, colClasses=c('character','character','numeric'))
  volts.raw <<- read.table(volts.filename, quote="\"")
  colnames(volts.raw)<<- c('ID','value')
  spikes.raw <<- read.table(spikes.filename, quote="\"")
  colnames(spikes.raw) <<- c('ID','value')
  settings <<- read.delim(settings.filename)
  
  GenerateAdjacencyMatrix()
  RenameIDs()
  UpdateData(settings)
}

CleanData <-function(){
  rm(nodes,connections,volts.raw,spikes.raw, envir=globalenv())
}