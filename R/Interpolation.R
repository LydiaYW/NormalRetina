#' Predict normative retina sensitivity
#' @param dt A numeric matrix from SensForFit.
#' @param ncpus A number.
#' @param cl A number.
#' @import graphics
#' @import gstat
#' @import sp
#' @import parallel
#' @import data.table
#' @examples
#' # Here is an example
#' Interpolation(ref77)
#' @export
Interpolation <- function(dt, ncpus=NULL, cl=NULL) {
  output <- list()
  # Calling parallel
  if (is.null(ncpus)) {
    ncpus <- max(1, parallel::detectCores() - 1)  # Use all but one core
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
    output <- parallel::parLapply(cl, unique(dt$Examtype), function(exam) {
      kriging_proc(dt, exam)
    })
  }

  return(output)
}
