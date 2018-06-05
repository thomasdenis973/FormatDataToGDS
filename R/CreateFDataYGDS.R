#' Create a FDataYGDS object to calculate IKA (Encounter Rate),and estimate abundance using gdistsamp function (package unmarked).
#'
#' @param data A FData object.
#' @param siteCovs A data.frame object with covariates. Warning !!! Sample name in line must be ordered as FData object.
#' @param samplingEffort A data.frame object with sampling effort created by CalculateSamplingEffort function.
#' @param nbReplicat A replicate number. Default value is 12.
#' @param stripWidth A distance value used to define stripwidth and truncate distance. Default value is 80.
#' @param DistanceClass A distance value used as distance interval of classes. Default value is 5.
#' @param unitsIn Either "m" or "km" defining the measurement units for both stripWidth and DistanceClass.
#'  See ? unmarkedFrameDS function (package unmarked). Default value is "m".
#' @param survey Either "point" or "line" for point- and line-transects.
#'  See ? unmarkedFrameDS function (package unmarked).
#' @param species Chosen species. Default value is "line".
#' @param groupNumber Logical. If TRUE, abundance is calculated using groups as observations.
#'  If FALSE, abundance is calculated using individuals as observations.
#' All values of 'number' variable of FData object are replaced by 1, including NA. Default value is TRUE.
#' @param naRmNumber Logical. If TRUE, observations which are not species number values (NA) are removed. Default value is FALSE.
#' @param naRmDistance Logical. If TRUE, observations which are not distance values (NA) are removed. Default value is FALSE.
#' @param missingValuesNumber Either method "mean" or "random". See details. Default value is "mean".
#' @param missingValuesDistance Either method "mean" or "random". See details. Default value is "mean".
#' @param ... other parameters of the unmarkedFrameGDS function(unmarked package)
#'
#' @details Method "mean" replaces a NA value by the mean value of the sample.
#' Method "random" replaces a NA value by a value drawn randomly from the sample.
#' @return Return a list with a FData object, a array with observations,
#' and an unmarkedFrameGDS object which can used in gdistsamp function (package unmarked).
#' @export
#'
#' @examples In progress
CreateFDataYGDS <- function(data, siteCovs=NULL, samplingEffort, nbReplicat=12, stripWidth=80, DistanceClass=5, unitsIn="m",survey="line", species="Cebus"
                           , groupNumber =TRUE, naRmNumber=FALSE, naRmDistance=FALSE, missingValuesNumber="mean", missingValuesDistance="mean", ...){

  if(length(levels(data$sample)) != length(colnames(samplingEffort))){
    stop("le nom des sample de data et samplingEffort ne sont pas de la meme longueur")
  }
  if(!all(levels(data$sample) == colnames(samplingEffort))){
    stop("le nom des sample de data et samplingEffort ne correspondent pas")
  }
  if(!species %in% data$species){
    stop("La species choisie n'est pas presents dans la variable species de data")
  }

  ### intervalles de distance
  breaks <- seq(0, stripWidth, by=DistanceClass)
  ### nombre de classes de distance
  J <- length(breaks)-1
  ## nombre of sample (site ou par exemple annéee)
  vSample <- levels(data$sample)
  R <- length(vSample)

  data2 <- data[data$species == species,]

  ### Remplacement des valeurs manquantes des nombres de Spcos: Valeurs moyennes
  if(groupNumber == TRUE){data2$number <- 1}
  if(naRmNumber == FALSE){data2$number <- ReplaceValue(data=data2,colName="number",method=missingValuesNumber)} else{data2 <- data2[!is.na(data2$number),]}
  if(naRmDistance == FALSE){data2$distance <- ReplaceValue(data=data2,colName="distance",method=missingValuesDistance)} else{data2 <- data2[!is.na(data2$distance),]}

  data3 <- data2

  ### Df des efforts par site et par répétition
  transectsLength <- colMeans(samplingEffort)

  ### array
  y <- array(0, c(R, J, nbReplicat))


  for(i in 1:R) {
    for(t in 1:nbReplicat) {

      selecSampleI <- data3[data3$sample==as.character(vSample[i]),]
      selecDistanceT <-  round(selecSampleI[selecSampleI$replicateNumber==t,]$distance,0)
      selecNumberT <- selecSampleI[selecSampleI$replicateNumber == t,]$number

      y[i,,t] <-
        sapply(1:J, function(x) {if(length(selecNumberT[(selecDistanceT %in% breaks[x]:(breaks[x+1]-1))==TRUE])==0) {0} else {
          sum(selecNumberT[(selecDistanceT %in% breaks[x]:(breaks[x+1]-1))==TRUE])}})
    }
  }

  lData <- list(FData=data2,y=y)


  y <- matrix(y, nrow=R) # convert array to matrix
  rownames(y) <- vSample
  lData$FrameGDS <- unmarked::unmarkedFrameGDS(y = y, siteCovs=siteCovs, survey=survey, unitsIn=unitsIn, dist.breaks=breaks, tlength=transectsLength, numPrimary=nbReplicat)
  lData$groupNumber = TRUE
  return(lData)

}


