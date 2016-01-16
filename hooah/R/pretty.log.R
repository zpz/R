pretty.log <-
function (x, n = 5) {
# Create a nice group of values that cover the range of X,
# where X has a long tail and therefore is being represented 
# on a log scale.
#
# The 'at' values returned is on the original X scale.
# Their logrithms are equi-spaced.

    xrange <- range(x)

    logbase <- 5
    repeat {
        at <- log(xrange, base = logbase)
        at <- floor(at[1]) : round(at[2])
        if (length(at) > n + 1) {
            logbase <- logbase + 5
        } else if (length(at) < n - 1) {
            if (logbase > 2) {
                logbase <- logbase - 1
            } else {
                warning(paste('Data do not have a large scale range.',
                    'Using a linear scale may be more appropriate.'))
                break
            }
        } else {
            break
        }
    }

    list(at = logbase ^ at, base = logbase)

}
