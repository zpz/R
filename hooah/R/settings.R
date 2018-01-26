
.pkgEnv <- new.env(parent = emptyenv())



.defaultOptions <- function()
{
    list(
        grid.par.settings = list(
                # grid.pars = list(cex = 10 / (par('ps') * par('cex')),
                #    lex = par('lwd')),
                strip.background = list(alpha = 0, col = 'transparent'),
                # strip.border = list(alpha = 0, col = 'transparent'),
                axis.components = list(
                    left = list(tck = .5), top = list(tck = .5),
                    right = list(tck = .5), bottom = list(tck = .5))
            )
            # Pass this to argument 'par.settings' of 'xyplot', etc.
            # When this function is evaluated, the device is already open.
            #
            # To find out what options are available for lattice
            # plots, check out
            #  trellis.par.get(), trellis.par.set(),
            #  lattice.options()
            #
            # To use smaller font for plots in publication,
            # pass in
            #   fontsize = list(text = 10, points = 8)
            # in 'par.settings'.
        )
}



#' @export
inquireEnv <- function(
    name = NULL,
        # Character vector of name(s) of variables to inquire about.
    envir
    )
{
    if (is.null(name))
        name <- ls(envir, all.names = TRUE)
    else
        stopifnot(is.character(name))

    z <- mget(name, envir, inherits = FALSE, ifnotfound = list(NULL))
    if (length(name) == 1L)
        z[[1]]
    else
        z
}



#' @export
modifyEnv <- function(
    ...,    # Items in 'name = value' format.
    envir
    )
{
    dots <- list(...)
    if (length(dots))
    {
        stopifnot(!is.null(names(dots)), names(dots) != '')
        for (name in names(dots))
        {
            val <- dots[[name]]

            if (name == 'parallel')
            {
                # Logical, or an integer indicating size of cluster
                # (or number of cores) to be used.
                if (!is.logical(val))
                {
                    stopifnot(
                        is.numeric(val),
                        trunc(val) == val,
                        val >= 0)
                    val <- as.integer(val)
                }
            } else if (name == 'localization.depth')
            {
                stopifnot(is.numeric(val), val >= 0)
                val <- as.integer(val)
            }

            if (exists(name, envir) && !is.null(val))
            {
                x <- get(name, pos = envir)
                if (is.list(val) && is.list(x))
                    val <- utils::modifyList(x, val)
            }
            assign(name, val, pos = envir, inherits = FALSE)
        }
    }

    invisible(NULL)
}



#' @export
getOptions <- function(...)
    inquireEnv(..., envir = .pkgEnv)



#' @export
setOptions <- function(...)
    modifyEnv(..., envir = .pkgEnv)


