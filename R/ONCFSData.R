#' Observations of a monkey species across 29 sites in French Guiana.
#'
#' A dataset containing the observations of a monkey species recorded
#' according to ONCFS sampling design. When no species or individual was observed on the subsample,
#'   a row was added. See details below.
#'
#' @format A data frame with 1614 rows and 10 variables:
#' \describe{
#'   \item{date}{date of the observation}
#'   \item{espece}{either "monkey" or "rodent"}
#'   \item{nombre}{number of individual observed in the field}
#'   \item{ precisementCompte}{"oui", when the group size had been precisely counted. Otherwise, "non}
#'   \item{distance}{perpendicular distance between observer and observation}
#'   \item{longueurLayon}{distance walked on the subsample (e.g., a transect)}
#'   \item{sens}{Either "Aller" or "Retour". Direction walked on the subsample}
#'   \item{site}{sample name}
#'   \item{layon}{subsample name. If there are not subsample, add sample name vector}
#'   \item{remarque}{"transect vide" significates that no species or individual was observed on the subsample.
#'   In this case, a row was added with sampling effort (and also date, direction and site) to take into account
#'   the "empty" subsample in the calculation of the sampling effort of the sample.
#'   when }
#'   ...
#' }
#' @source French Guianan ONCFS Cecile Richard-Hansen
"ONCFSData"
