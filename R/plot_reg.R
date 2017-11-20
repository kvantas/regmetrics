
#' Create a plot of observed versus predicted values
#'
#' \code{plotPO} creates an observed versus predicted plot. The use of this plot
#' is to check if predictions are uniformly accurate. The diagonal grey reference
#'  line indicates where the observed and predicted values would be equal.
#'
#' @param observed   observed  numeric vector.
#' @param predicted  predicted numeric vector.
#'
#' @return a ggplot class
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  plotPO(y_obs, y_pred)

plotPO <- function(observed, predicted) {

    if (!check_vectors(observed, predicted))
        return(NULL)

    data <- data.frame('observed' = observed, 'predicted' = predicted)

    minVal <- min(c(data$observed, data$predicted))
    maxVal <- max(c(data$observed, data$predicted))


    g1 <- ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = predicted, y = observed)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, col = "grey8", linetype = 5) +
        ggplot2::labs(x = "predicted", y = "observed") +
        ggplot2::xlim(minVal, maxVal) + ggplot2::ylim(minVal, maxVal) +
        ggplot2::theme_bw()

    return(g1)
}

#' Create a plot of predicted versus residuals
#'
#' \code{plotPR} creates an observed versus residuals plot. The use of this plot
#' is to help uncover systematic patterns in the model predictions. The
#' horizontal grey reference line indicates the residuals when the observed and
#' predicted values would be equal.
#'
#' @inheritParams plotPO

#'
#' @return a ggplot class
#' @export
#'
#' @examples
#'  y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
#'              0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
#'              0.55, -1.30, -1.15, 0.20)
#'  y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
#'                0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
#'               -0.25, -0.64, -1.26, -0.07)
#'  plotPR(y_obs, y_pred)
plotPR <- function(observed, predicted) {

    if (!check_vectors(observed, predicted))
        return(NULL)

    residuals <- observed - predicted

    g1 <- ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x= predicted,y = residuals)) +
        ggplot2::geom_abline(intercept = 0, slope = 0, col = "grey8", alpha = 0.9, linetype = 5) +
        ggplot2::labs(x = "predicted", y = "residuals") +
        ggplot2::theme_bw()

    return(g1)

}
