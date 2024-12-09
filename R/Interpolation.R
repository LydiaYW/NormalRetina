#' Predict normative retina sensitivity
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @export
#' @import graphics
#' @import gstat
#' @import sp
#' @import parallel
#' @import data.table
#' @examples
#' # Here is an example
#'
Interpolation <- function(dt, exam) {
  suppressMessages(suppressWarnings({

    if(!(exam %in% c("Mesopic", "Cyan", "Red", "CRdiff"))){
      stop("Please indicate the type of exam (Mesopic, Cyan, Red or CRdiff)")
    }

    output <- mclapply(unique(dt$Examtype), function(exam) {
      set.seed(123)

      interpol_dat <- dt[dt$Examtype == exam]
      coordinates(interpol_dat) <- ~x + y

      # Fit variogram with grid search for optimal parameters
      min_rmse <- Inf
      best_psill <- NULL
      best_range <- NULL

      # Search for optimal parameters
      for (psill in seq(10, 30, by = 1)) {
        for (range in seq(3, 10, by = 0.5)) {
          vgm_model <- vgm(psill = psill, model = "Sph", range = range)
          vgm_fit <- try(fit.variogram(variogram(MeanSens ~ 1, interpol_dat), model = vgm_model), silent = TRUE)
          if (inherits(vgm_fit, "try-error")) next

          # Cross-validation to calculate RMSE
          kriging_cv <- try(krige.cv(MeanSens ~ 1, interpol_dat, model = vgm_fit, nfold = 10), silent = TRUE)
          if (inherits(kriging_cv, "try-error")) next

          rmse <- sqrt(mean((kriging_cv$var1.pred - kriging_cv$observed)^2, na.rm = TRUE))

          if (rmse < min_rmse) {
            min_rmse <- rmse
            best_psill <- psill
            best_range <- range
          }
        }
      }

      if (is.null(best_psill) | is.null(best_range)) {
        stop(paste("Could not find optimal variogram parameters for exam type:", exam))
      }

      # Fit the variogram with the optimal parameters
      vgm <- variogram(MeanSens ~ 1, interpol_dat)

      if (exam == "CRdiff") {
        vgm_fit <- fit.variogram(vgm, vgm(model = "Ste", psill = best_psill, range = best_range), fit.kappa = TRUE, fit.method = 1) # CRdiff uses a stable model
      } else {
        vgm_fit <- fit.variogram(vgm, vgm(model = "Sph", psill = best_psill, range = best_range), fit.kappa = TRUE, fit.ranges = TRUE, fit.sills = TRUE, fit.method = 1) # Others are spherical
      }

      kriging_result <- krige(MeanSens ~ 1, interpol_dat, grid, model = vgm_fit)
      list(
        data = data.table(Examtype = exam, X_corr = grid$x, Y_corr = grid$y, mean = kriging_result$var1.pred),
        min_rmse = min_rmse,
        best_psill = best_psill,
        best_range = best_range
      )
    }, mc.cores = detectCores())
    return(output)
  }))
}
