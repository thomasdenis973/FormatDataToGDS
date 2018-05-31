#' Represent observation number by distance class.
#'
#' @param data A FDataYGDS object created by CreateFDataYGDS function.
#' @param ...
#'
#' @return Return a plot of observation number (in y) by distance class (in x).
#' @export
#'
#' @examples In progress
PlotObservationPerDistanceClass <- function(data,...){

  distanceClass <- sapply(1:(length(data$FrameGDS@dist.breaks)-1),function(x) paste(data$FrameGDS@dist.breaks[x],"-",data$FrameGDS@dist.breaks[x+1],sep=""))

  df <- data.frame(ObservationNumber=rowSums(colSums(data$y)),
                   distanceClass=1:length(rowSums(colSums(data$y))),
                   distanceClassLabel=as.character(distanceClass))
  #df$distanceClass <- as.factor(levels(df$distanceClass))


  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=distanceClass, y=ObservationNumber)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::scale_x_continuous(breaks=seq(1,16,1),labels=as.character(df$distanceClassLabel))
  return(p)
}

