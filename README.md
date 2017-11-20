<!-- README.md is generated from README.Rmd. Please edit that file -->
regmetrics
==========

The goal of regmetrics is to provide a set of regression metrics for measuring performance in Regression Models and plots for observed, predicted values and residuals from models.

Example
-------

This is a basic example:

``` r
library(regmetrics)
data("cars")

# create a linear model
lm1 <- lm(dist~speed, data = cars)

# create vectors of observed and predicted values
y_obs <- cars$dist
y_pred <- fitted(lm1)

# compute error metrics
print(paste('R2 = ',r2(y_obs, y_pred)))
#> [1] "R2 =  0.651079380758251"
print(paste('RMSE = ',rmse(y_obs, y_pred)))
#> [1] "RMSE =  15.0688559957914"
print(paste('MAE = ',mae(y_obs, y_pred)))
#> [1] "MAE =  11.5801191240876"

# Create a plot of observed versus predicted values
plotPO(y_obs, y_pred)
```

![](README-example-1.png)

``` r

# Create a plot of predicted versus residuals
plotPR(y_obs, y_pred)
```

![](README-example-2.png)
