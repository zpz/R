
boxcox <- function(
    x,
    lambda,     # Scalar.
    tol = 0.02)
{
    if (abs(lambda) < tol)
        log(x)
    else
        (x^lambda - 1) / lambda
}




## This function calculates the proportion of the Gaussian distribution
## of the Box-Cox transformed variable that would back-transform with no
## problem.
## Not used for now.
boxcox.plausibility <- function(
    lambda, beta = 0, eta2,
    trend.func,
    location
        # Location is a matrix of coordiates of points of interest,
        # can be the entire grid, an sub-area of interest,
        # or a single point, e.g. the central location.
        # If multiple locations are passed in,
        # this function may do some averaging
        # or simply take the central point.
    )
{
    if (lambda == 0)
        return(1)

    x <- (-1/lambda - c(trend.func(location) %*% beta)) / sqrt(eta2)
    if (lambda > 0)
        1 - mean(pnorm(x))
    else
        mean(pnorm(x))
}




## Inverse Box-Cox transformation.
boxcox.rev <- function(
    x,
    lambda,     # Scalar.
    tol = 0.02)
{
    if (abs(lambda) < tol)
        exp(x)
    else
        (lambda * x + 1)^(1/lambda)
}



