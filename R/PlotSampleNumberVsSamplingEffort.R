#' Represent total sampling effort according to sample number.
#'
#' @param data A FData object.
#' @param seqReplicat A numeric vector with replicate numbers
#'
#' @return Return a plot with the sampling effort in y and the sample number in x. Lables are the replicate number.
#' @export
#'

PlotSampleNumberVsSamplingEffort <- function(data,seqReplicat){
  df <- data.frame(SampleNumber=rep(NA,length(seqReplicat)),samplingEffort=rep(NA,length(seqReplicat)))

  for(i in 1:length(seqReplicat))
  {
    vReplicatePerSubSample <-  ReplicatePerSubSample(data)
    data2 <- data[data$subSample %in% names(vReplicatePerSubSample[vReplicatePerSubSample >= seqReplicat[i]]),]
    data2 <- data2[data2$replicateNumber <= seqReplicat[i],]
    data2 <- gdata::drop.levels(data2)
    df[i,] <- c(length(unique(data2$sample)),sum(CalculateSamplingEffort(data2))/1000)
  }


  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=df$SampleNumber, y=df$samplingEffort)) +
    #ggplot2::geom_point()+
    ggplot2::geom_label(ggplot2::aes(label = seqReplicat))
  return(p)

}
