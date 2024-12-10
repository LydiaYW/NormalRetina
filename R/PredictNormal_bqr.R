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
PredictNormal_bqr <- function(dt, exam="Mesopic", model="LMM", CalibSplit=0.2, coverage=0.95 #,
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
    adjusted_q <- coverage*(1+1/nrow(calib))
    Q_E <- quantile(E,probs=adjusted_q, type=1)
    test_lower <- predict(qgam_l, newdata = test)-Q_E
    test_upper <- predict(qgam_u, newdata = test)+Q_E
    qgam_observed_coverage <- mean(test$MeanSens >= test_lower & test$MeanSens <= test_upper)
    qgam_mace <- abs(qgam_observed_coverage - coverage)

    # Store results
    cv_mae[[i]] <- data.frame(fold = i, mae = qgam_mae, model = "bqr")
    cv_mace[[i]] <- data.frame(
      fold = i,
      model = c("bqr"),
      mace = c(qgam_mace),
      observed_coverage = c(qgam_observed_coverage)
    )
  }
  cv_mae_tab <- rbindlist(cv_mae)
  cv_mace_tab <- rbindlist(cv_mace)
  return(list(cv_mae_tab,cv_mace_tab))
}
