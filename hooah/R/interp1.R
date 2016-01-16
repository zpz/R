interp1 <-
function(x, y = NULL, xout, ...) {
# Linear interpolation;
# out-of-range points are extrapolated.
# This function is written specifically for the extrapolation functionality.
# Otherwise, the base function 'approx' suffices.
# If extrapolation is undesirable, use 'approx' instead.

    xy <- approx(x, y, xout, ...)

    xmin <- min(x)
    xmax <- max(x)

    if (any(xy$x <= xmin) || any(xy$x >= xmax)) {
        if (is.null(y)) {
            x <- xy.coords(x, NULL)
            y <- x$y
            x <- x$x
        }

        xminidx <- which.min(x)
        xmin <- x[xminidx]
        xmaxidx <- which.max(x)
        xmax <- x[xmaxidx]

        idx <- which(xy$x <= xmin)
        if (any(idx)) {
            xminy <- y[xminidx]
            xsubminidx <- which.min(x[-xminidx])
            xsubmin <- x[-xminidx][xsubminidx]
            xsubminy <- y[-xminidx][xsubminidx]

            xy$y[idx] <- (xminy * (xsubmin - xy$x[idx]) +
                xsubminy * (xy$x[idx] - xmin)) / (xsubmin - xmin)
        }

        idx <- which(xy$x > xmax)
        if (any(idx)) {
            xmaxy <- y[xmaxidx]
            xsubmaxidx <- which.max(x[-xmaxidx])
            xsubmax <- x[-xmaxidx][xsubmaxidx]
            xsubmaxy <- y[-xmaxidx][xsubmaxidx]

            xy$y[idx] <- (xsubmaxy * (xmax - xy$x[idx]) +
                xmaxy * (xy$x[idx] - xsubmax)) / (xmax - xsubmax)
        }
    }

    xy
}
