#' Calculate sampling effort of sample for each replicate
#'
#' @param data A FData object
#'
#' @return Return data.frame object with sampling effort of sample for each replicate
#' @export
#'
#' @examples In progress
CalculateSamplingEffort <- function(data)
{
  replicateNumber <-  max(data$replicateNumber)
  dfSamplingEffort <- matrix(NA,replicateNumber,length(levels(data$sample)))

  vSample <- levels(data$sample)

  for (i in vSample)
    for( j in 1:replicateNumber)
    {
      selec <- data[data$sample == i & data$replicateNumber == j,]
      dfSamplingEffort[j,which(vSample == i)] <- sum(selec[sapply(unique(selec$subSample), function(x) which(selec$subSample  == x)[1]),]$samplingEffort) ### calcul longueur / rep
    }
  colnames(dfSamplingEffort) <- vSample
  return(dfSamplingEffort)
}

