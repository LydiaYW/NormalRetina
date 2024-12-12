#' Predict normative retina sensitivity
#' @param model A string.
#' @param dt A numeric matrix from SensForFit.
#' @param age A number
#' @export
#' @import qgam
#' @import lme4
#' @import ranger
#' @import stats
#' @import data.table
#' @examples
#' # Here is an example
#' PredictSensitivity("LMM", 77, refMes)
#' @export
PredictSensitivity <- function(model, age, dt) {
  if (!model %in% c("LMM","BQR","RF")) {
    stop("Please select your best model from LMM, BQR and RF")
  }

  # Create an expanded grid
  grid <- expand.grid(x = seq(-20, 20, by = 0.05), y = seq(-20, 20, by = 0.05))
  grid$eccentricity <- sqrt(grid$x^2 + grid$y^2)

  coordinates(grid) <- ~x + y
  gridded(grid) <- TRUE

  grid_df <- as.data.table(grid)
  grid_df[, `:=`(
    angle = atan2(y, x) * (180 / pi), # Calculate angle in degrees
    Eye = factor("OS", levels = c(levels(dt$Eye), "OS")),
    Age = age
  )]

  # Check model type and predict
  if (model=="LMM") {
    lmm <- lmer(MeanSens ~ Age + eccentricity + (1 | Patient), data = dt)
    grid_df[, `:=`(
      Patient = factor("new", levels = c(levels(dt$Patient), "new"))
    )]
    grid_df[, mean := predict(lmm, newdata = .SD, allow.new.levels = TRUE), .SDcols = c("Age", "eccentricity", "Patient")]

  } else if (model=="BQR") {
    bqr <- qgam(MeanSens ~ s(x, y, k=30) + s(Age, k=3) +
                  ti(x,Age, k=3) + ti(y,Age, k=3),
                data = dt, qu=0.5)
    grid_df[, mean := predict(bqr, newdata = .SD), .SDcols = c("Age", "x", "y")]
  } else if (model=="RF") {
    rf <- ranger::ranger(
      formula = MeanSens ~ Age + x + y + eccentricity + angle + Eye,
      data = dt,
      num.trees = 500,
      mtry = floor(sqrt(ncol(dt) - 1)),
      quantreg = TRUE                     # Enable quantile regression
    )
    # Predict with RF
    predictions <- predict(rf, data = grid_df)
    grid_df[, mean := predictions$predictions]
  }

  return(grid_df[, .(X_corr = x, Y_corr = y, mean)])
}
