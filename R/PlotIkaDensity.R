#' Represent relatioship between IKA (i.e., Encounter rate, 10km walked length) and
#' estimated density.
#'
#' @param data A FDataYGDS object created by CreateFDataYGDS function.
#' @param ... other parameters of the ggplot function (ggplot2 package)
#'
#' @return Return a plot of estimated density (in x) and IKA (in x).
#' @export
#'
#' @examples In progress
PlotIkaDensity <- function(data, ...){
  df<-base::data.frame(IKA=data$IKA,Density=data$Density)
  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=IKA, y=Density)) +
    ggplot2::geom_point()
  return(p)
}
