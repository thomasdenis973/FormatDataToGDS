#' Give the replicate number per subsample
#'
#' @param data A FData object
#'
#' @return return a vector with the replicate number per subsample
#' @export
#'

ReplicatePerSubSample <- function(data){
  return(unlist(lapply(sapply(utils::unstack(data, data$replicateNumber~data$subSample), unique),length)))
}

