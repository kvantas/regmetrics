
#' Check regression vectors
#'
#' \code{check_reg_vectors} checks that x is a numeric vector without NA values
#'
#' @param x
#'
#' @return if x is a numeric vector without NA values return TRUE. Otherwise the
#' output will be FALSE
#' @export
#'
#' @examples
#' check_reg_vectors(NULL)
#' check_reg_vectors(list(1, 2, 3))
#' check_reg_vectors(data.frame(x=c(1, 2, 3)))
#' check_reg_vectors(c(1, 2, 3))
check_reg_vectors <- function(x) {

    # check for NULL values
    if (is.null(x))
        return(FALSE)

    # check if x is numeric without NA values
    return(is.vector(x, "numeric") & !any(is.na(x)))
}



check_vectors_len <- function(x, y) {
  return(length(x) == length(y))
}


#' Compute the total sum of squares
#'
#' \code{tss} returns the sum, over all observations, of the squared differences
#' of each observation from the overall mean.
#'
#' \deqn{TSS = \sum _{i=1}^n {(y_i - \bar y)^2}}
#' where \eqn{\bar y} is the overal mean
#'
#' @param observed ground truth numeric vector
#'
#' @return If all observations are numeric, then the output will be a number.
#' Otherwise the output will be an integer.
#' @export
#'
#' @examples
tss <- function(observed) {
    sum((observed - mean(observed))^2)
}

#' Compute the residual sum of squares
#'
#'  It is defined as the sum of the squares of residuals, deviations predicted
#'  from actual empirical values of data. It is a measure of the discrepancy
#'  between the data and an estimation model
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return
#' @export
#'
#' @examples
rss <- function(observed, predicted) {
    sum((observed - predicted)^2)
}

#' Coefficient of determination
#'
#' It provides a measure of how well future samples are likely to be predicted
#' by the model. Best possible score is 1.0 and it can be negative (because the
#' model can be arbitrarily worse). A constant model that always predicts the
#' expected value of y, disregarding the input features, would get a R^2
#' score of 0.0.
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return
#' @export
#'
#' @examples
rsquared <- function(observed, predicted) {
    1 - rss(observed, predicted)/tss(observed)
}


#' Root-mean-square error
#'
#'  Is a frequently used measure of the differences between values (sample and
#'  population values) predicted by a model or an estimator and the values
#'  actually observed.
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return
#' @export
#'
#' @examples
rmse <- function(observed, predicted) {
    sqrt(mean(rss(observed, predicted)))
}


#' Mean absolute error
#'
#' Mean absolute error is a risk metric
#' corresponding to the expected value of the absolute error loss or l1-norm
#' loss.
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return
#' @export
#'
#' @examples
mae <- function(observed, predicted) {
    mean(abs(observed - predicted))
}


#' Explained variance score
#'
#' It provides a measure of how well future samples are likely to be predicted
#' by the model. The Explained variance best possible score is 1.0, lower
#' values are worse.
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return
#' @export
#'
#' @examples
exp_var_score <- function(observed, predicted) {
    1 - var(observed - predicted)/var(observed)
}


#' Mean squared logarithmic error
#'
#' It provides a risk metric corresponding to
#' the expected value of the squared logarithmic (quadratic) error or loss.
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return
#' @export
#'
#' @examples
msle <- function(observed, predicted) {
    mean(sum(log(1 + observed) - log(1 + predicted)))
}


#' Mean bias error
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' It describes the direction of the error bias. Its value, however, is related
#' to magnitude of values under investigation. A negative MBE occurs when
#' predictions are smaller in value than observations.
#'
#' @return
#' @export
#'
#' @examples
mbe <- function(observed, predicted) {
    mean(observed - predicted)
}
