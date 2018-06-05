#' Create FData object by formatting dataframe and adding replicate number according to samping effort.
#'
#' @param species A vector of species name.
#' @param number A vector of number of observed individuals.
#' @param preciselyCounted A vector indicating when the observed individuals were precisely counted.
#' @param distance A vector of perpendicular distance from the transect to the observation (individual or group).
#' @param sample A vector of sample name (site, year, etc).
#' @param subSample A vector of subsample name used to be aggregate to sample level.
#' @param order A vector using to define replicates number.
#' @param samplingEffort A vector of sampling effort for each replicate: number of m walked by the observer for the replicat
#'
#' @return Return a FData object: data.frame object with formated column names.
#' Replicate numbers are given according to sampling effort. Replicate numbers are ordered acoording to sampling effort.
#' First replicate numbers have the highest sampling effort whereas the lastest are the smallest.
#' Use CalculateSamplingEffort function to see sampling effort values per replicate.
#' @export
#'
#' @examples In progess
CreateFData <- function(species,number,preciselyCounted,distance,sample,subSample,order,samplingEffort){

  data <- data.frame(species=species,number=number,preciselyCounted=preciselyCounted,distance=distance,sample=sample,subSample=subSample,order=order,samplingEffort=samplingEffort)
  data <- gdata::drop.levels(data)

  if(!is.data.frame(data)){
    stop("L'objet data doit etre un data.frame")
  }
  if(!is.numeric(samplingEffort)){
    stop("L'objet samplingEffort doit etre numeric")
  }


  data2 <- matrix(NA,0,ncol(data))
  vSubSample <- levels(data$subSample)
  print("Hardly working ")
  for (i in vSubSample)
  {
    dataSubSample <- data[order(data$order),]

    replicateNumber <- 1
    dataSubSample <- data[subSample == i,]
    dataSubSample$Unsampled <- max(dataSubSample$samplingEffort) - dataSubSample$samplingEffort
    dataSubSample <- dataSubSample[order(dataSubSample$Unsampled, dataSubSample$order),] #df),]
    dataSubSample$replicateNumber <- NA
    dataSubSample$replicateNumber[1] <-  replicateNumber


    cat(".")

    for (j in 2:nrow(dataSubSample))
    {

      replicateNumber <- if(dataSubSample$subSample[j] == dataSubSample$subSample[j-1] & dataSubSample$order[j] == dataSubSample$order[j-1])
      {replicateNumber} else {replicateNumber+1}
      dataSubSample$replicateNumber[j] <- replicateNumber


    }
    data2 <- rbind(data2,dataSubSample)
  }

  return(data2)

}
