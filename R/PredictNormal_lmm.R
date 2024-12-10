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
PredictNormal_lmm <- function(dt, exam="Mesopic", model="LMM", CalibSplit=0.2, coverage=0.95 #,
                              # other_predict = NULL
){
  nFold <- max(dt$fold)
  cv_mae <- list()
  cv_mace <- list()

  for (i in 1:nFold) {
    # Train/calibration/test split
    train <- dt[dt$fold != i,]
    set.seed(123+i)
    calibration <- sample(unique(train$Patient), CalibSplit * length(unique(train$Patient)))
    calib <- train[train$Patient %in% calibration,]
    train <- train[!train$Patient %in% calibration,]
    test <- dt[dt$fold == i,]

    # Fit lmer model
    lm0 <- lmer(MeanSens ~ Age + (1|Patient), data = train)
    calib_pred <- predict(lm0, newdata = calib, allow.new.levels = TRUE)
    calib_residuals <- abs(calib$MeanSens - calib_pred)
    q_lm0 <- quantile(calib_residuals, coverage)
    test_pred <- predict(lm0, newdata = test, allow.new.levels = TRUE)
    lm0_mae <- mean(abs(test_pred - test$MeanSens))

    lm0_lower <- test_pred - q_lm0 # Lower bound of 95% CI
    lm0_upper <- test_pred + q_lm0  # Upper bound of 95% CI
    lm0_observed_coverage <- mean(test$MeanSens >= lm0_lower & test$MeanSens <= lm0_upper)
    lm0_mace <- abs(lm0_observed_coverage - nominal_coverage)

    # Store results
    cv_mae[[i]] <- data.frame(fold = i, mae = lm0_mae, model = "lmm")
    cv_mace[[i]] <- data.frame(
      fold = i,
      model = c("lmm"),
      mace = c(lm0_mace),
      observed_coverage = c(lm0_observed_coverage)
    )
  }
  cv_mae_tab <- rbindlist(cv_mae)
  cv_mace_tab <- rbindlist(cv_mace)
  return(list(cv_mae_tab,cv_mace_tab))
}
