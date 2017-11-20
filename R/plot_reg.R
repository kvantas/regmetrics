
## function plotPAR creates predicted-observed plot and predicted-residuals plots
plotPAR <-function(pred,
                   obs,
                   ylab = 'Προβλέψεις',
                   xlab = 'Παρατηρήσεις',
                   xlabRES = 'Παρατηρήσεις',
                   ylabRES = 'Υπόλοιπα',
                   Col = 'grey',
                   Alpha = 0.3,
                   rYmin = -2500,
                   rYmax = 3100,
                   span = 0.5,
                   n = 100,
                   Log = TRUE,
                   xmin = 1,
                   xmax = 11000,
                   ymin= 1,
                   ymax = 11000) {
    df <- data.frame('pred' = pred,
                     'obs' = obs,
                     'res' = obs - pred)

    g1 <- ggplot(data = df, aes(x = obs, y = pred)) +
        geom_point(alpha =Alpha, col = Col) +
        geom_abline(intercept = 0,
                    slope = 1,
                    col = 'black',
                    alpha = 0.9,
                    linetype = 5) +
        stat_smooth(
            method = "loess",
            col = 'red',
            se = FALSE,
            alpha = 0.1,
            linetype = 5,
            span = span,
            n = n
        ) +
        labs(x = xlab, y = ylab) +
        xlim(xmin,xmax) +
        ylim(ymin,ymax) +
        theme_bw()

    if(Log) {
        g1 <- g1 + scale_x_log10()+ scale_y_log10()
    }

    g2 <- ggplot(data = df, aes(x = obs, y = res)) +
        geom_point(alpha = Alpha, col = Col) +
        # scale_x_log10()+
        geom_abline(
            intercept = 0,
            slope = 0,
            col = 'black',
            alpha = 0.9,
            linetype = 5
        ) +
        stat_smooth(
            method = "loess",
            col = 'red',
            se = FALSE,
            alpha = 0.9,
            linetype = 5) +
        ylim(rYmin, rYmax) +
        labs(x = xlabRES, y = ylabRES) +
        theme_bw()

    if(Log) {
        g2 <- g2 + scale_x_log10()
    }

    return(list('obsPred' = g1,
                'obsRes' = g2))
}
