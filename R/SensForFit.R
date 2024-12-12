#' Sensitivity data pre-processing
#' @param dt A numeric matrix contains all information.
#' @param examcol Column name for exam type.
#' @param idcol Column name for patient ID.
#' @param agecol Column name for patient age.
#' @param senscol Column name for outcome.
#' @param k A number of folds
#' @import stats
#' @import data.table
#' @examples
#' # Here is an example
#'
#' @export
SensForFit <- function(dt, examcol, idcol, agecol, senscol, k){
  setDT(dt)

  setnames(dt, old = c(idcol, agecol, examcol, senscol),
           new = c("Patient", "Age", "Examtype", "MeanSens"))

  # filter exam type
  dt <- dt[Examtype %in% c("Mesopic", "Cyan", "Red", "CRdiff")]

  # remove negative MeanSens, convert Patient to factor, and Age to numeric
  dt[, MeanSens := fifelse(MeanSens >= 0, MeanSens, 0)]
  dt[, Patient := as.factor(Patient)]
  dt[, Age := as.numeric(Age)]

  # patient-wise split
  dt[, fold := sample(1:k, .N, replace = TRUE), by = .(Patient)]

  return(dt)
}

