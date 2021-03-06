% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_reg.R
\name{plotPR}
\alias{plotPR}
\title{Create a plot of predicted versus residuals}
\usage{
plotPR(observed, predicted)
}
\arguments{
\item{observed}{observed  numeric vector.}

\item{predicted}{predicted numeric vector.}
}
\value{
a ggplot class
}
\description{
\code{plotPR} creates an observed versus residuals plot. The use of this plot
is to help uncover systematic patterns in the model predictions. The
horizontal grey reference line indicates the residuals when the observed and
predicted values would be equal.
}
\examples{
 y_obs  <- c(0.22,  0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
             0.62,  0.99, -0.18, 0.32,  0.34, -0.30,  0.04, -0.87,
             0.55, -1.30, -1.15, 0.20)
 y_pred <- c(0.24,  0.78, -0.66,  0.53, 0.70, -0.75, -0.41, -0.43,
               0.49,  0.79, -1.19,  0.06, 0.75, -0.07,  0.43, -0.42,
              -0.25, -0.64, -1.26, -0.07)
 plotPR(y_obs, y_pred)
}
