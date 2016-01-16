#' Report size of all objects in an environment.
#' The environment defaults to be the one within which this function is
#' called.
#'
#' @examples
#'
#' function(x) {
#'    # .... do things
#'    print(objects.size())
#'      # Check, during debugging,
#'      # what objects take up most memory space.
#' }
#'
#' x <- list(x = runif(30000), y = runif(400))
#' with(x, objects.size())
#'
objects.size <- function(envir = parent.frame())
# For global envir, pass in 'envir = .GlobalEnv'
{
    obs <- ls(envir = envir)
    return(rev(sort(
        sapply(obs, function (object.name)
            object.size(get(object.name, envir = envir)) / 1048600
                # object size in MB.
            ))))
}
