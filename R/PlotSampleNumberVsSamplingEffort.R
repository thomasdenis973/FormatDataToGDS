#' Represent total sampling effort according to sample number.
#'
#' @param data A FData object.
#' @param seqReplicat A numeric vector with replicate numbers
#'
#' @return Return a plot with the sampling effort in y and the sample number in x. Lables are the replicate number.
#' @export
#'
#' @examples In progress
PlotSampleNumberVsSamplingEffort <- function(data,seqReplicat){
  df <- data.frame(SampleNumber=rep(NA,length(seqReplicat)),samplingEffort=rep(NA,length(seqReplicat)))

  for(i in 1:length(seqReplicat))
  {
    data <- dfRawData2
    vReplicatePerSubSample <-  ReplicatePerSubSample(data)
    data <- data[data$subSample %in% names(vReplicatePerSubSample[vReplicatePerSubSample >= seqReplicat[i]]),]
    data <- data[data$replicateNumber <= seqReplicat[i],]
    data <- gdata::drop.levels(data)
    df[i,] <- c(length(unique(data$sample)),sum(CalculateSamplingEffort(data))/1000)
  }


  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=SampleNumber, y=samplingEffort)) +
    #ggplot2::geom_point()+
    ggplot2::geom_label(ggplot2::aes(label = seqReplicat))
  return(p)

}
