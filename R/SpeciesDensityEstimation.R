#' Estimate species density using gdistsamp function (package unmarked).
#'
#' @param data A FDataYGDS object.
#' @param modelGDS Either "P" or "NB" for the Poisson and negative binomial models of abundance. "NB" by default. See ? gdistsamp.
#' @param methodGDS Optimization method used by optim. "BFGS" by default.
#' @param keyfunGDS One of the following detection functions: "halfnorm", "hazard", "exp", or "uniform".
#' "halfnorm" by default. See ? gdistsamp.
#' @param ... This function used CreateDataYGDS function with default arguments. See ? CreateDataYGDS.
#'
#' @return Return a list with an object of class unmarkedFitGD, a vector with IKA (Encounter rate for 10 km),
#'  a vector with abundance in km^{2} estimated by ranef function (package unmarked).
#' @export
#'
#' @examples In progress
SpeciesDensityEstimation <- function(data,modelGDS="NB",methodGDS="BFGS",keyfunGDS="halfnorm",...){

  ### inférence du modèle
  gdistsampSpeciesI <- gdistsamp(~1,~1,~1, data$FrameGDS, ### data: $GDSData de l'objet DataYGDS
                                 keyfun = keyfunGDS,unitsOut = "kmsq",
                                 output="density", mixture = modelGDS,K=80,method=methodGDS)

  ### estimation de la variable latente: abundance de la superpopulation en nbre de GROUPES
  MSpeciesI <- bup(ranef(gdistsampSpeciesI,stat="mean"))

  ### estimation de la migration temporaire phi
  phiSpeciesI <- 1/(1+exp(-gdistsampSpeciesI@estimates@estimates$phi@estimates))

  ### estimation de l'abondance de la population en nbre de GROUPES
  NSpeciesI <- MSpeciesI*phiSpeciesI

  IKASpeciesI <- (rowSums(data$y)/(data$FrameGDS@tlength*data$FrameGDS@numPrimary))*10000

  return(list(model=gdistsampSpeciesI,
              IKA=IKASpeciesI,
              Density=NSpeciesI))

}
