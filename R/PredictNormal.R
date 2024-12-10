#' Predict normative retina sensitivity
#' @param model A string.
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @param CalibSplit A number.
#' @param coverage A number.
#' @export
#' @import qgam
#' @import lme4
#' @import ranger
#' @import stats
#' @examples
#' # Here is an example
#'
#'
PredictNormal <- function(
    dt, exam="Mesopic", model="LMM", CalibSplit=0.2, coverage=0.95 #,
    # other_predict = NULL
    ){
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

  if(model=="LMM"){
    PredictNormal_lmm(dt, exam, CalibSplit, coverage)
  } else if (model == "QBC"){
    PredictNormal_bqr(dt, exam, CalibSplit, coverage)
  } else if (model == "RF"){
    PredictNormal_rf(dt, exam, CalibSplit, coverage)
  }

}
