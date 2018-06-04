PlotIkaDensity <- function(data){
  df<-data.frame(IKA=data$IKA,Density=data$Density)
  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=IKA, y=Density)) +
    ggplot2::geom_point()
  return(p)
}
