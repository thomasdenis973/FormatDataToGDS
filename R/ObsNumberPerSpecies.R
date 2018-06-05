#' Calculate observation number per species.
#'
#' @param data A FData object.
#'
#' @return return a vector with observation number per species.
#' @export
#'

ObsNumberPerSpecies <- function(data){
  vSpeciesName <- unlist(lapply(levels(data$species),function(i) {
    length(utils::unstack(data,data$number~data$species)[[i]])})
  )
  names(vSpeciesName) <- levels(data$species)
  return(vSpeciesName)
}
