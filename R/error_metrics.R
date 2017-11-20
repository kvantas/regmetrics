# Compute the total sum of squares
TSS <- function(observed) {
    sum((observed - mean(observed))^2)
}


#' Compute the residual sum of squares
#'
#' \code{RSS} returns the sum of the squares of residuals, deviations predicted
#'  from observed values of data.
#'
#' \deqn{RSS = \sum_{i=1}^n{y_i - \hat {y_i}}}
#' where \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @param observed  observed  numeric vector.
#' @param predicted predicted numeric vector.
#'
#' @return  A numeric vector of length one.
#'
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  RSS(y_obs, y_pred)
RSS <- function(observed, predicted) {
    sum((observed - predicted)^2)
}



#' Coefficient of determination
#'
#' \code{r2} returns a measure of how well future samples are likely to be
#' predicted by the model. Best possible score is 1.0 and it can be negative
#' (because the model can be arbitrarily worse). A constant model that always
#' predicts the expected value of y, disregarding the input features, would get
#' a score of 0.0.
#'
#' \deqn{R^{2} = 1 - \frac{RSS}{TSS}}
#' where:
#' \deqn{RSS = \sum_{i=1}^n{y_i - \hat {y_i}}}
#' \deqn{TSS = \sum _{i=1}^n {(y_i - \bar y)^2}}
#' and \eqn{\bar y} is the overal mean and \eqn{\hat {y_i}} is the predicted
#' value of \eqn{y_i}.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{(-\infty ,1]} if
#' \code{observed} and \code{predicted} are numeric vectors without NA values
#' and the same length. Otherwise the output will be \code{NA}.

#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#' r2(y_obs, y_pred)

r2 <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(1 - RSS(observed, predicted)/TSS(observed))
    } else {
        return(NA)
    }
}


#' Explained variance score
#'
#' \code{expl_var} returns a measure of how well future samples are likely to be
#' predicted by the model. The Explained variance best possible score is 1.0,
#' lower values are worse.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{(-\infty ,1]} if
#' \code{observed} and \code{predicted} are numeric vectors without NA values,
#' with the same length. Otherwise the output will be \code{NA}.
#'
#' \deqn{EVS = 1 - \frac{var(\hat {y_i} - y_i)}  {var(y_i)}}
#' where \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#' expl_var(y_obs, y_pred)
expl_var <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(1 - var(observed - predicted)/var(observed))
    } else {
        return(NA)
    }
}


#' Root-mean-square error
#'
#' \code{rmse} returns the square root of the average of squared errors
#' between values predicted by a model and the values actually observed. It
#' corresponds to the expected value of the squared error loss or l2-norm loss.
#'
#' \deqn{RMSE = \sqrt {\frac{1}{n}\sum_{i=1}^n(y_i- \hat{y_i})^2}}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{[0, +\infty)} if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  rmse(y_obs, y_pred)

rmse <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(sqrt(mean((observed - predicted)^2)))
    } else {
        return(NA)
    }
}



#' Mean absolute error
#'
#' \code{mae} returns the the average of absolute errors between values
#' predicted by a model and the values actually observed. It corresponds to the
#' expected value of the absolute error loss or l1-norm loss.
#'
#' \deqn{MAE = \frac{1}{n}\sum_{i=1}^n|y_i- \hat{y_i}|}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{[0, +\infty)} if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.

#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  mae(y_obs, y_pred)

mae <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(mean(abs(observed - predicted)))
    } else {
        return(NA)
    }
}



#' Mean squared logarithmic error
#'
#' \code{msle} returns the the average of absolute errors corresponding to
#' the expected value of the squared logarithmic (quadratic) error or loss.
#'
#' \deqn{MSLE = \frac{1}{n} \sum_{i=1}^{n} (\log_e (1 + y_i) -
#'       \log_e (1 + \hat{y}_i) )^2}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{[0, +\infty)} if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  msle(y_obs, y_pred)

msle <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        logdiff <- log(1 + observed) - log(1 + predicted)
        return(mean(sum(logdiff)^2))
    } else {
        return(NA)
    }
}



#' Mean bias error
#'
#' \code{mde} returns the direction of the error bias. Its value is related
#' to the magnitude of values under investigation. A negative MBE occurs when
#' predictions are smaller in value than observations.
#'
#' \deqn{MBE = \frac{1}{n} \sum_{i=1}^n(y_i- \hat{y_i})}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in
#' \eqn{(-\infty), +\infty)} if \code{observed} and  \code{predicted} are
#' numeric vectors without NA values with the same length. Otherwise the output
#' will be \code{NA}.
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  mbe(y_obs, y_pred)

mbe <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(mean(predicted - observed))
    } else {
        return(NA)
    }

}



#' Normalized squared error
#'
#' \code{nse} returns the normalized mean squared error
#' between values predicted by a model and the values actually observed.
#'
#' \deqn{ NSE = \frac{MSE(y_{i}, \hat {y_i})}{MSE(y_{i}, 0)}}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{[0, +\infty)} if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.
#'
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  nse(y_obs, y_pred)
nse <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(RSS(observed, predicted)/RSS(observed, 0))
    } else {
        return(NA)
    }

}

#' Normalized absolute error
#'
#' \code{nae} returns the normalized  absolute error
#' between values predicted by a model and the values actually observed.
#'
#' \deqn{ NAE = \frac{MAE(y_{i}, \hat {y_i})}{MAE(y_{i}, 0)}}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.

#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{[0, +\infty)} if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.
#'
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  nae(y_obs, y_pred)
nae <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(RSS(observed, predicted)/RSS(observed, 0))
    } else {
        return(NA)
    }

}

#' Normalized bias error
#'
#' \code{nbe} returns the normalized mean absoluye error
#' between values predicted by a model and the values actually observed.
#'
#' \deqn{ NAE = \frac{MBE(y_{i}, \hat {y_i})}{MBE(y_{i}, 0)}}
#' where  \eqn{\hat {y_i}} is the predicted value of \eqn{y_i}.
#
#'
#' @inheritParams RSS
#'
#' @return A numeric vector of length one. A number in \eqn{[0, +\infty)} if
#' \code{observed} and  \code{predicted} are numeric vectors without NA values
#' with the same length. Otherwise the output will be \code{NA}.
#'
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  nbe(y_obs, y_pred)
nbe <- function(observed, predicted) {
    if (check_vectors(observed, predicted)) {
        return(sum(observed - predicted)/sum(observed))
    } else {
        return(NA)
    }

}
