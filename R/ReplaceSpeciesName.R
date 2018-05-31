#' Replace species names by other term
#'
#' @param speciesNames A vector of taxon names that are to be replaced
#' @param taxonReplacement A taxon name use to replace taxon names
#' @param speciesVector A vector of species names provided by FData object
#'
#' @return Return a vector of species names to replace species variable of dataframe created by CreateFData function
#' @export
#'
#' @examples In progress
ReplaceSpeciesName <- function(speciesNames,taxonReplacement,speciesVector){
  return(
    as.factor(
      sapply(1:length(speciesVector), function(x) if(speciesVector[x] %in% speciesNames){taxonReplacement} else{as.character(speciesVector[x])})
    )
  )
}
