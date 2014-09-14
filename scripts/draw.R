PlotCompleteAdjacencyMatrix <- function(matrix){
  m <- matrix
  levelplot(m, col.regions=rainbow(3), aspect='iso', at=c(-1.5,-0.5,0.5,1.5),
            xlab="Pre-synaptic", ylab="Post-synaptic",
            colorkey=list(at=c(-1.5,-0.5,0.5,1.5),
                          labels=list(at=c(-1.0,0.0,1.0),
                                      labels=c("Inhibitory", "None", "Excitatory"))),
            border="black")
}

PlotDifferenceMatrix <- function(matrix){
  m <- matrix
  levelplot(m, col.regions=colorRampPalette(c('red','green','purple')), aspect='iso', at=c(-1.5,-0.5,0.5,1.5),
            xlab="Pre-synaptic", ylab="Post-synaptic",
            colorkey=list(at=c(-1.5,-0.5,0.5,1.5),
                          labels=list(at=c(-1.0,0.0,1.0),
                                      labels=c("False negative", "Correct", "False positive"))),
            border="black")
}

PlotDifferenceTypeMatrix <- function(matrix){
  m <- matrix
  levelplot(m, col.regions=colorRampPalette(c('red','green','purple')), aspect='iso', at=c(-1.5,-0.5,0.5,1.5),
            xlab="Pre-synaptic", ylab="Post-synaptic",
            colorkey=list(at=c(-1.5,-0.5,0.5,1.5),
                          labels=list(at=c(-1.0,0.0,1.0),
                                      labels=c("In when ex", "Correct", "Ex when in"))),
            border="black")
}

PlotTotalDifferenceMatrix <- function(matrix){
  m <- matrix
  levelplot(m, col.regions=colorRampPalette(c('green','blue','purple','orange','yellow')), aspect='iso', at=c(-0.5,0.5,1.5,
                                                                                                              2.5,3.5,4.5),
            xlab="Pre-synaptic", ylab="Post-synaptic",
            colorkey=list(at=c(-0.5,0.5,1.5,
                               2.5,3.5,4.5),
                          labels=list(at=c(0.0,1.0,2.0,3.0,4.0),
                                      labels=c("Correct", "False negative", "False positive",
                                               "Ex when in",
                                               "In when ex"))),
            border="black")
}

PlotSimpleAdjacencyMatrix <- function(matrix){
  m <- matrix
  levelplot(m, col.regions=rainbow(2), aspect='iso', at=c(-0.5,0.5,1.5), colorkey=list(tick=2), border="black")
}

PlotRealValueMatrix <- function(matrix){
  m <- matrix
  levelplot(m, at=seq( min(m), max(m), length=length(as.vector(m))^2), xlab="Pre-synaptic", ylab="Post-synaptic",
            col.regions=colorRampPalette(c('black','red','yellow','white')), aspect='iso')
}

LmPlotSpikes <- function(fit){
  if(attr(fit$terms,'intercept')==1){
    coefs = coef(fit)[-1]
  }else{
    coefs = coef(fit)
  }
  at.vector <- c(fit$autoregression.size/2)
  label.vector <- c("Auto\nregressive")
  i=0
  for(neuron in nodes$name){
    at.vector = c(at.vector, fit$autoregression.size+fit$window.size/2+i*fit$window.size)
    label.vector = c(label.vector,neuron)
    i = i+1
  }
  par(mar=c(7,4,4,4))
  plot(coefs, xlab="", type="h", xlim=c(-1,length(coefs)+1), xaxt="n", lwd=1)
  axis(1,at=at.vector, labels=label.vector, las=2)
  
  abline(v=0, col="red")
  abline(v=fit$autoregression.size, col="red")
  for(i in seq_len(length(nodes$name))){
    abline(v=i*fit$window.size+1+fit$autoregression.size, col="red")
  }
}

CompareDelaysBarPlot <- function(delays){
  t <- delays$Inferred-delays$Real
  x <- barplot(t, yaxt='n', col=c("darkblue"), ylab="Error [ms]", xlab="Connection", ylim=c(-0.4, 0.15))
  axis(side=2,at=seq(-35,15,by=5)/100)
  text(cex=1, x=x, y=pmin(t,0)-0.008*sapply(as.character(delays$Name),nchar), delays$Name, xpd=TRUE, srt=90)
}