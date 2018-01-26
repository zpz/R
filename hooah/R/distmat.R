#' Euclidean or Mahalanobis dist matrix between rows of two matrices.
#'
#' @export
distmat <- function(
    x,
    y,
    Sigma,
    inverted = FALSE,
    return.squared = FALSE
    )
{
    if (!is.matrix(x) || ncol(x) == 1)
    {
        dim(x) <- NULL
        nx <- length(x)

        if (missing(y))
        {
            #z <- outer(x, x, '-')
            z <- x - rep(x, each = nx)
            dim(z) <- c(nx, nx)
        } else
        {
            if (is.matrix(y))
            {
                stopifnot(ncol(y) == 1)
                dim(y) <- NULL
            }
            ny <- length(y)
            #z <- outer(x, y, '-')
            z <- x - rep(y, each = nx)
            dim(z) <- c(nx, ny)
        }

        if (!missing(Sigma))
        {
            dim(Sigma) <- NULL
            if (!inverted)
                Sigma <- 1/Sigma
            if (return.squared)
                z * z * Sigma
            else
                abs(z) * sqrt(Sigma)
        } else
        {
            if (return.squared)
                z * z
            else
                abs(z)
        }
    } else
    {
        nx <- nrow(x)
        p <- ncol(x)

        if (missing(Sigma))
        {
            # The squared distance between vectors 'a' and 'b' is
            #     t(a - b) %*% (a - b)
            #   = t(a) %*% a + t(b) %*% b - 2 * t(a) %*% b

            xx <- .rowSums(x * x, nx, p)

            if (missing(y))
            {
                xtx <- tcrossprod(x)
                #z <- outer(xx, xx, '+') - 2*xtx
                z <- xx + rep(xx, each = nx) - 2 * xtx
            } else
            {
                stopifnot(is.matrix(y), ncol(y) == p)
                ny <- nrow(y)

                yy <- .rowSums(y * y, ny, p)
                xy <- tcrossprod(x, y)
                #z <- outer(xx, yy, '+') - 2*xy
                z <- xx + rep(yy, each = nx) - 2 * xy
            }
        } else
        {
            if (missing(y))
            {
                if (inverted)
                    xtx <- tcrossprod(x %*% Sigma, x)
                else
                    xtx <- x %*% solve(Sigma, t(x))

                xx <- diag(xtx)

                #z <- outer(xx, xx, '+') - 2*xtx
                z <- xx + rep(xx, each = nx) - 2*xtx
            } else
            {
                stopifnot(is.matrix(y), ncol(y) == p)
                ny <- nrow(y)

                if (!inverted)
                    Sigma <- chol2inv(chol(Sigma))
                xx <- .rowSums((x %*% Sigma) * x, nx, p)
                yy <- .rowSums((y %*% Sigma) * y, ny, p)
                xy <- tcrossprod(x %*% Sigma, y)

                #z <- outer(xx, yy, '+') - 2*xy
                z <- xx + rep(yy, each = nx) - 2*xy
            }
        }

        # The i'th row contains the Mahalanobis distance
        # of the i'th row of 'x' to each row of 'y'.

        z[z <= 0] <- 0

        if (return.squared)
            z
        else
            sqrt(z)
    }
}
