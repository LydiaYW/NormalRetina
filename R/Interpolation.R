#' Predict normative retina sensitivity
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @param ncpus A number.
#' @param cl A number.
#' @import graphics
#' @import gstat
#' @import sp
#' @import parallel
#' @import data.table
#' @examples
#' # Here is an example
#'
#' @export
Interpolation <- function(dt, exam, ncpus=NULL, cl=NULL) {
    if(!(exam %in% c("Mesopic", "Cyan", "Red", "CRdiff"))){
      stop("Please indicate the type of exam (Mesopic, Cyan, Red or CRdiff)")
    }

    # Calling parallel
    if ((ncpus > 1) | length(cl)) {

      # Creating the cluster
      if (!length(cl)) {
        cl <- parallel::makeCluster(ncpus)
        on.exit(parallel::stopCluster(cl))

        # Loading R packages
        parallel::clusterEvalQ(cl, library(netdiffuseR))
      }
    }

    output <- parLapply(cl, unique(dt$Examtype), function(exam){
      kriging_proc(dt, exam)
    })
}
