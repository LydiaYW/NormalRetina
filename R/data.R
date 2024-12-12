#' Example Dataset for a reference patient aged 77
#'
#' @format A data frame with 2566404 rows and 6 variables:
#' \describe{
#'   \item{Examtype}{A string (Mesopic or Cyan or Red).}
#'   \item{eccentricity}{Stimulus eccentricity.}
#'   \item{angle}{Stimulus angel.}
#'   \item{x}{x coordinate.}
#'   \item{y}{y coordinate.}
#'   \item{MeanSens}{mean sensitivity.}
#' }
#' @usage data(ref77)
#' @source Virtual sensitivity data
"ref77"

#' Example Dataset for a reference population
#'
#' @format A data frame with 7445 rows and 14 variables:
#' \describe{
#'   \item{Patient}{Patient ID.}
#'   \item{Eye}{Study eye.}
#'   \item{Examtype}{A string (Mesopic or Cyan or Red).}
#'   \item{Age}{Patient age.}
#'   \item{eccentricity}{Stimulus eccentricity.}
#'   \item{angle}{Stimulus angel.}
#'   \item{x}{x coordinate.}
#'   \item{y}{y coordinate.}
#'   \item{MeanSens}{mean sensitivity.}
#'   \item{fold}{fold for cross validation.}
#' }
#' @usage data(refMes)
#' @source Virtual sensitivity data
"refMes"
