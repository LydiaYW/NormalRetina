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
#' @import dplyr
#' @import stats
#' @examples
#' # Here is an example
#'
PredictNormal <- function(dt, exam="Mesopic", model="LMM", CalibSplit=0.2, coverage=0.95){
  # check arguments
  if(!(model %in% c("LMM", "BQR", "RF"))){
    stop("Model opitions: LMM, BQR, RF")
  }

  if(!(exam %in% c("Mesopic", "Cyan", "Red"))){
    stop("Please indicate the type of exam (Mesopic, Cyan or Red)")
  }

  # check data format
  if(!is.character(dt[,1])){
    stop("Please include ID in first column")
  }

  nFold <- max(dt$fold)
  cv_mae <- list()
  cv_mace <- list()
  #----LMM----
  if(model=="LMM"){
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
  }

  #----BQR----
  if(model=="BQR"){
    for (i in 1:nFold) {
      # Train/calibration/test split
      train <- dt[dt$fold != i,]
      set.seed(123+i)
      calibration <- sample(unique(train$Patient), CalibSplit * length(unique(train$Patient)))
      calib <- train[train$Patient %in% calibration,]
      train <- train[!train$Patient %in% calibration,]
      test <- dt[dt$fold == i,]

      # Fit qgam model
      #train <- dt[dt$fold != i,]
      qgam <- qgam(MeanSens ~ s(x, y, k=30) + s(Age, k=3) +
                     ti(x,Age, k=3) + ti(y,Age, k=3),
                   data = train, qu=0.5)
      qgam_pred <- predict(qgam, newdata = test)
      qgam_mae <- mean(abs(qgam_pred - test$MeanSens))

      qgam_l <- qgam(MeanSens ~ s(x, y, k = 30) + s(Age, k = 3) +
                       ti(x, Age, k = 3) + ti(y, Age, k = 3),
                     data = train, qu = (1-coverage)/2)
      qgam_u <- qgam(MeanSens ~ s(x, y, k = 30) + s(Age, k = 3) +
                       ti(x, Age, k = 3) + ti(y, Age, k = 3),
                     data = train, qu = 1-(1-coverage)/2)
      qgam_lower <- predict(qgam_l, newdata = calib)
      qgam_upper <- predict(qgam_u, newdata = calib)
      E <- pmax(qgam_lower-calib$MeanSens, calib$MeanSens-qgam_upper)
      adjusted_q <- nominal_coverage*(1+1/nrow(calib))
      Q_E <- quantile(E,probs=adjusted_q, type=1)
      test_lower <- predict(qgam_l, newdata = test)-Q_E
      test_upper <- predict(qgam_u, newdata = test)+Q_E
      qgam_observed_coverage <- mean(test$MeanSens >= test_lower & test$MeanSens <= test_upper)
      qgam_mace <- abs(qgam_observed_coverage - nominal_coverage)

      # Store results
      cv_mae[[i]] <- data.frame(fold = i, mae = qgam_mae, model = "bqr")
      cv_mace[[i]] <- data.frame(
        fold = i,
        model = c("bqr"),
        mace = c(qgam_mace),
        observed_coverage = c(qgam_observed_coverage)
      )
    }
  }

  #----RandomForest----
  if(model=="RF"){
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
      nominal_coverage <- coverage

      # MACE calculation
      fold_mace <- abs(observed_coverage - nominal_coverage)

      # Store results
      cv_mae[[i]] <- data.frame(fold = i, mae = mean(abs((lower_bound + upper_bound) / 2 - test$MeanSens)), model = "random forest")
      cv_mace[[i]] <- data.frame(fold=i, mace=fold_mace, model="random forest")
    }
  }

  cv_mae_tab <- bind_rows(cv_mae)
  cv_mace_tab <- bind_rows(cv_mace)

}
