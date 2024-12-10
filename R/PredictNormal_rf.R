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
#' @export
PredictNormal_rf <- function(dt, exam="Mesopic", model="LMM", CalibSplit=0.2, coverage=0.95 #,
                              # other_predict = NULL
){
  nFold <- max(dt$fold)
  cv_mae <- list()
  cv_mace <- list()

  for (i in 1:nFold) {
    # Train-test split
    train <- dt[dt$fold != i,]
    set.seed(123+i)
    calibration <- sample(unique(train$Patient), CalibSplit * length(unique(train$Patient)))
    calib <- train[train$Patient %in% calibration,]
    train <- train[!train$Patient %in% calibration,]
    test <- dt[dt$fold == i,]

    # Fit random forest with quantile regression
    rf <- ranger::ranger(
      formula = MeanSens ~ Age + x + y + eccentricity + angle + Eye,
      data = train,
      num.trees = 500,
      mtry = floor(sqrt(ncol(train) - 1)),
      quantreg = TRUE                     # Enable quantile regression
    )

    # Predict quantiles for calibration set
    calib_pred <- predict(rf, data=calib)[["predictions"]]
    calib_residuals <- abs(calib$MeanSens - calib_pred)
    quantiles <- quantile(calib_residuals, coverage)
    test_pred <- predict(rf, data = test)[["predictions"]]
    lower_bound <- test_pred-quantiles  # 2.5th percentile
    upper_bound <- test_pred+quantiles  # 97.5th percentile

    # Calculate observed coverage
    observed_coverage <- mean(test$MeanSens >= lower_bound & test$MeanSens <= upper_bound)

    # Nominal coverage
    coverage <- coverage

    # MACE calculation
    fold_mace <- abs(observed_coverage - coverage)

    # Store results
    cv_mae[[i]] <- data.frame(fold = i, mae = mean(abs((lower_bound + upper_bound) / 2 - test$MeanSens)), model = "random forest")
    cv_mace[[i]] <- data.frame(fold=i, mace=fold_mace, model="random forest")
  }
  cv_mae_tab <- rbindlist(cv_mae)
  cv_mace_tab <- rbindlist(cv_mace)
  return(list(cv_mae_tab,cv_mace_tab))
}
