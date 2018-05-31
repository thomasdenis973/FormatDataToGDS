#' Give the replicate number per subsample
#'
#' @param data A FData object
#'
#' @return return a vector with the replicate number per subsample
#' @export
#'
#' @examples In progess
ReplicatePerSubSample <- function(data){
  return(unlist(lapply(sapply(unstack(data, replicateNumber~subSample), unique),length)))
}

