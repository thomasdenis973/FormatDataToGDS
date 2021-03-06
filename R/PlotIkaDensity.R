#' Represent relatioship between IKA (i.e., Encounter rate, 10km walked length) and
#' estimated density.
#'
#' @param data A FDataYGDS object created by CreateFDataYGDS function.
#' @param ... other parameters of the ggplot function (ggplot2 package)
#'
#' @return Return a plot of estimated density (in x) and IKA (in x).
#' @export
#'

PlotIkaDensity <- function(data, ...){
  df <- data.frame(IKA=data$IKA,Density=data$Density)
  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=df$IKA, y=df$Density)) +
    ggplot2::geom_point()
  return(p)
}
