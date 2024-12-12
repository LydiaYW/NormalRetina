#' Example Dataset for a reference patient aged 77
#'
#' @format A data frame with 2566404 rows and 6 variables:
#' \describe{
#'   \item{Examtype}{A string (Mesopic or Cyan or Red).}
#'   \item{X_corr}{x coordinate.}
#'   \item{Y_corr}{y coordinate.}
#'   \item{mean}{mean sensitivity.}
#'   \item{lwr}{lower bound of sensitivity.}
#' }
#' @usage data(ref77)
#' @source Virtual sensitivity data
"ref77"

#' Example Dataset for a reference population
#'
#' @format A data frame with 7445 rows and 14 variables:
#' \describe{
#'   \item{Patient}{Patient ID.}
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
