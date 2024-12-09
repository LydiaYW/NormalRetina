#' Predict normative retina sensitivity
#' @param model A string.
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @export
#' @import graphics
#' @examples
#' # Here is an example
#'
Interpolation <- function(dt, exam, model){
  if(!(exam %in% c("Mesopic", "Cyan", "Red"))){
    stop("Please indicate the type of exam (Mesopic, Cyan or Red)")
  }

}
