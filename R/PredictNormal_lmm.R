#' Predict normative retina sensitivity - lmm
#' @param model A string.
#' @param dt A numeric matrix from SensForFit.
#' @param exam A string.
#' @param CalibSplit A number.
#' @param coverage A number.
#' @export
#' @import lme4
#' @import stats
#' @export
PredictNormal_lmm <- function(dt, exam="Mesopic", model="LMM", CalibSplit=0.2, coverage=0.95){
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
    lm0 <- lmer(MeanSens ~ Age + eccentricity + (1|Patient), data = train)
    calib_pred <- predict(lm0, newdata = calib, allow.new.levels = TRUE)
    calib_residuals <- abs(calib$MeanSens - calib_pred)
    q_lm0 <- quantile(calib_residuals, coverage)
    test_pred <- predict(lm0, newdata = test, allow.new.levels = TRUE)
    lm0_mae <- mean(abs(test_pred - test$MeanSens))

    lm0_lower <- test_pred - q_lm0 # Lower bound of 95% CI
    lm0_upper <- test_pred + q_lm0  # Upper bound of 95% CI
    lm0_observed_coverage <- mean(test$MeanSens >= lm0_lower & test$MeanSens <= lm0_upper)
    lm0_mace <- abs(lm0_observed_coverage - coverage)

    # Store results
    cv_mae[[i]] <- lm0_mae
    cv_mace[[i]] <- lm0_mace
  }
  # Compute overall metrics
  cv_mae_overall <- mean(unlist(cv_mae))
  cv_mace_overall <- mean(unlist(cv_mace))
  return(c("LMM", cv_mae_overall, cv_mace_overall))
}
