#' Predict normative retina sensitivity with spacial interpolation
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @param ncpus A number.
#' @param cl A number.
#' @import graphics
#' @import gstat
#' @import sp
#' @import parallel
#' @import data.table
#' @export
Interpolation <- function(dt, exam="Mesopic", ncpus=NULL, cl=NULL) {
  output <- list()
  # Calling parallel
  if (is.null(ncpus)) {
    ncpus <- max(1, parallel::detectCores() - 1)
  }

  if (is.null(cl) && ncpus > 1) {
    cl <- parallel::makeCluster(ncpus)
    on.exit(parallel::stopCluster(cl))  # Ensure the cluster is stopped on exit
  }

  if (!is.null(cl)) {
    parallel::clusterEvalQ(cl, {
      library(NormalRetina)
      library(gstat)
      library(sp)
      library(data.table)
    })# Load the required package
    parallel::clusterExport(cl, varlist = c("dt", "kriging_proc"), envir = environment())
  }

  # Perform computation
  if (!is.null(cl)) {
    # Parallel computation
    output <- parallel::parLapply(cl, unique(dt$Examtype), function(exam1) {
      message("Processing exam: ", exam)
      kriging_proc(dt, exam1)
    })
  }

  return(output)
}
