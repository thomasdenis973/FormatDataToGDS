#' Calculate the number of truncated observation per sample.
#'
#' @param data A FDataYGDS object created by CreateFDataYGDS function
#'
#' @return Return the number of truncated observation per sample.
#' @export
#'
#' @examples In progress
TruncatedObservation <- function(data){

  return(sapply(utils::unstack(data$FData,number~sample),sum,na.rm=TRUE) - rowSums(data$y))
}
