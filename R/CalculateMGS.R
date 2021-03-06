#' Calculate the mean group size per sample.
#'
#' @param data A FData object.
#' @param species A species name.
#' @param threshold A number of up to which the mean group size is calculated from a sample.
#' Otherwise, mean group size is calculated from all samples.
#' @param sampleName A vector of sample names in the same order as FDataYGDS object.
#' @param preciselyCountedValue A character value.
#'
#' @return return a vector of mean group size per sample.
#' @export
#'

CalculateMGS <- function(data,species,threshold,sampleName,preciselyCountedValue){
  vector <- rep(NA,length(sampleName)) ###

  for(i in sampleName)
  {
    data2 <- data[data$species == species,]
    NumberI <- data2[as.character(data2$sample) == i & data2$preciselyCounted == preciselyCountedValue,]$number
    vector[which(sampleName ==i)] <- if(length(NumberI) > threshold) {

      mean(NumberI,na.rm=TRUE)

    } else  {
      mean(data2[data2$preciselyCounted == preciselyCountedValue,]$number,na.rm = TRUE)}
  }
  names(vector) <- sampleName
  return(vector)
}
