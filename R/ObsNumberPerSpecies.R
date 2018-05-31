#' Calculate observation number per species.
#'
#' @param data A FData object.
#'
#' @return return a vector with observation number per species.
#' @export
#'
#' @examples In progress
ObsNumberPerSpecies <- function(data){
  vSpeciesName <- unlist(lapply(levels(data$species),function(i) {
    length(unstack(data,number~species)[[i]])})
  )
  names(vSpeciesName) <- levels(data$species)
  return(vSpeciesName)
}
