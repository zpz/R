hist.cdf.log <-
function (cdfx, cdfy, plot = TRUE, axes = TRUE,
    ylab = ifelse(axes, 'Relative frequency', ''), ...) {
# Plot a histogram given the empirical CDF curve.
# Use log scale for X because this distribution has a long tail.

    h <- hist.cdf(log(cdfx), cdfy, plot = plot, axes = FALSE, ...)

    xtcklab <- pretty.log(cdfx)$at
        # X tick mark labels
    xtckat <- interp1(h$breaks, h$axbreaks, log(xtcklab))$y
        # X tick mark locations

    if (plot && axes) {
        axis(1, at = xtckat, labels = xtcklab)
        axis(2)
        title(ylab = ylab)
    }

    h$xtckat <- xtckat
    h$xtcklab <- xtcklab

    invisible(h)
}


hist.cdf <-
function (cdfx, cdfy, breaks = 12, plot = TRUE,
    axes = TRUE, ylab = ifelse(axes, 'Relative frequency', ''), ...) {
# Plot a histogram given the empirical CDF curve.

    if (length(breaks) == 1) {
        xy <- approx(cdfx, cdfy, n = breaks + 1)
    } else {
        xy <- approx(cdfx, cdfy, xout = breaks)
    }

    nbin <- length(xy$x) - 1

    prob <- diff(xy$y)
    axtick <- pretty(xy$x)
    xtckat <- interp1(xy$x, 0 : nbin, axtick)$y
    xtcklab <- axtick

    if (plot) {
        barplot(prob, space=0, col='white',
            axes = axes, ylab = ylab, ...)
            # The range of X axis enclosing the bars is [0, nbin], linearly.
            # [0, nbin] linearlly corresponds to range(cdfx), i.e., range(xy$x).
        if (axes) axis(1, at = xtckat, labels = xtcklab)
    }

    h <- list()
    h$breaks <- xy$x
    h$axbreaks <- 0 : nbin
    h$prob <- prob
    h$mids <- (xy$x[1 : nbin] + xy$x[2 : (nbin + 1)]) / 2
    h$axmids <- (0 : (nbin - 1)) + 0.5
    h$xtckat <- xtckat
    h$xtcklab <- xtcklab

    invisible(h)
}


hist.log <-
function (x, plot = TRUE, ...) {
# Histogram with log scale on X.
# The log scale is used because of the distribution's long tail.
#
# See help for the base function 'hist'.

    h <- hist(log(x), plot = plot, axes = FALSE, ...)

    xtcklab <- pretty.log(x)$at
        # X tick mark labels in the original unit
    xtckat <- log(xtcklab)
        # X tick mark locations on the log scale

    if (plot) {
        axis(1, at = xtckat, labels = prettyNum(xtcklab, digits=4))
        axis(2)
    }

    h$xtckat <- xtckat
    h$xtcklab <- xtcklab

    invisible(h)
}


