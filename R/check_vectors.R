#' Check regression vectors
#'
#' \code{check_num} checks that \code{x} is a numeric vector without
#' \code{NA} values.
#'
#' @param x an input
#'
#' @return if \code{x} is a numeric vector without NA values return \code{TRUE}.
#' Otherwise the output will be \code{FALSE}.
#'
#' @export
#'
#' @examples
#' # x is NULL
#' check_num(NULL)
#'
#' # x is a list
#' check_num(list(1, 2, 3))
#'
#' # x is a dataframe
#' check_num(data.frame(x=c(1, 2, 3)))
#'
#' # x is a numeric vector with NA
#' check_num(c(1, 2, NA))
#'
#' # x is a  numeric vector
#' check_num(1:4)

check_num <- function(x) {

    # check for NULL values
    if (is.null(x))
        return(FALSE)

    # check if x is numeric without NA values
    return(is.vector(x, "numeric") & !any(is.na(x)))
}



#' Check vectors' lenghts
#'
#' \code{check_len} checks that x and y have the same length

#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return if x and y have the same length return TRUE. Otherwise the
#' output will be FALSE
#'
#'@export
#'
#' @examples
#'
#' # same length
#' x <- 1:3
#' y <- 2:4
#' check_len(x, y)
#'
#'# different length
#' x <- 1:4
#' y <- 1:2
#' check_len(x, y)
check_len <- function(x, y) {
    return(length(x) == length(y))
}


#' Check observed and predicted vectors
#'
#' \code{check_vectors} checks if observed and predicted vectors are numeric
#' without NA values and that have the same length.
#'
#' @param observed  ground truth number or vector
#' @param predicted predicted number or vector
#'
#' @return TRUE if the two vectors are valid, otherwise false.
#'
#' @export
#'
#' @examples
#'  y_obs  <- c(3.0, -0.5, 2.0, 5.0)
#'  y_pred <- c(3.9, -0.2, 2.8, 4.5)
#'  check_vectors(y_obs, y_pred)

check_vectors <- function(observed, predicted) {
    return(check_num(observed) & check_num(predicted) & check_len(observed, predicted))
}

