#' Predict normative retina sensitivity
#' @param model A string.
#' @param dt A numeric matrix.
#' @param exam A string.
#' @export
#' @import graphics
#' @examples
#' # Here is an example
#'
PredictNormal <- function(dt, exam="Mesopic", model="LMM"){
  # check arguments
  if(!(model %in% c("LMM", "BQR", "RF"))){
    stop("Model opitions: LMM, BQR, RF")
  }

  if(!(exam %in% c("Mesopic", "Cyan", "Red"))){
    stop("Please indicate the type of exam (Mesopic, Cyan or Red)")
  }

  # check data format
  if(!is.character(dt[,1])){
    stop("Please include ID in first column")
  }


}
