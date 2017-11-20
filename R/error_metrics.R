# Total sum of squares
TSS <- function(observed) {
    sum((observed - mean(observed))^2)
}

# Residual sum of squares
RSS <- function(observed, predicted) {
    sum((observed - predicted)^2)
}

# Mean squared error
mse <- function(observed, predicted) {
    mean((observed - predicted)^2)
}

# Coefficient of determination
r2 <- function(observed, predicted) {
    1 - RSS(observed, predicted)/TSS(observed)
}

# Root-mean-square error
rmse <- function(observed, predicted) {
    sqrt(mse(observed, predicted))
}

# Mean absolute error
mae <- function(observed, predicted) {
    mean(abs(observed - predicted))
}

# Root Mean squared logarithmic error
rmsle <- function(observed, predicted) {

    # check for negative values
    if (any(observed + 1 <= 0) | any(1 + predicted <= 0)) {
        return(NaN)
    }

    logdiff <- log(1 + observed) - log(1 + predicted)
    return(sqrt(mean(logdiff)^2))
}

# Mean bias error
mbe <- function(observed, predicted) {
    mean(predicted - observed)
}


# Normalized bias error
nbe <- function(observed, predicted) {
    sum((observed - predicted)/observed)
}

#' Compute regression error metric
#'
#' \code{error_metric} returns a selected error metric between values predicted
#' by a model and the values actually observed.
#'
#' The available error metrics are:
#' \itemize{
#'   \item Root-mean-square error, \code{rmse}.
#'   \item Mean absolute error, \code{mae}.
#'   \item Mean bias error, \code{mbe}.
#'   \item Coefficient of determination, \code{r2}.
#' }
#'
#' @param observed  observed  numeric vector.
#' @param predicted predicted numeric vector.
#' @param metric    one of 'RMSE', 'MAE', 'R2', 'MBE'.
#'
#' @return A numeric vector of length one. A number if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.
#' @export
#'
#' @examples
#'
#' y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'             0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'             0.55, -1.30, -1.15, 0.20)
#' y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'             0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'            -0.25, -0.64, -1.26, -0.07)
#' # RMSE
#' error_metric(y_obs, y_pred, metric = 'rmse')
#'
#' # MAE
#' error_metric(y_obs, y_pred, metric = 'mae')
#'
#' # R2
#' error_metric(y_obs, y_pred, metric = 'R2')
#'
#' # MBE
#' error_metric(y_obs, y_pred, metric = 'mbe')
error_metric <- function(observed, predicted, metric = c("RMSE", "MAE", "R2", "MBE")) {

    metric <- match.arg(metric)

    if (!check_vectors(observed, predicted))
        return(NA)

    switch(metric,
           RMSE = rmse(observed, predicted),
           MAE = mae(observed, predicted),
           R2 = r2(observed, predicted),
           MBE = mbe(observed, predicted))

}
