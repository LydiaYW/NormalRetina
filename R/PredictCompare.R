#' Compare normative retina sensitivity prediction models
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @param CalibSplit A number.
#' @param coverage A number.
#' @import qgam
#' @import lme4
#' @import ranger
#' @import stats
#' @examples
#' # Here is an example
#' PredictCompare(refMes, exam="Mesopic", CalibSplit=0.2, coverage=0.95)
#' @export
PredictCompare <- function(
    dt, exam="Mesopic", CalibSplit=0.2, coverage=0.95){
  # check arguments
  if(!(exam %in% c("Mesopic", "Cyan", "Red"))){
    stop("Please indicate the type of exam (Mesopic, Cyan or Red)")
  }


  lmm_results <- PredictNormal_lmm(dt, CalibSplit, coverage)

  bqr_results <- PredictNormal_bqr(dt, CalibSplit, coverage)

  rf_results <- PredictNormal_rf(dt, CalibSplit, coverage)

  # Combine results
  tab.comp <- rbind(lmm_results, bqr_results, rf_results)
  colnames(tab.comp) <- c("Model", "MAE", "MACE")

  return(tab.comp)

}
