#' Check whether an object is of \code{field.ensemble} class.
#'
#' @export
is.field.ensemble <- function(object)
{
    inherits(object, 'field.ensemble')
}



#' Construct a \code{field.emsemble} object.
#'
#' @export
field.ensemble <- function(fields, grid)
{
    structure(list(fields = fields, grid = grid), class = 'field.ensemble')
}



#' Plot a \code{field.ensemble} object.
#'
#' @param x
#'      An \code{field.ensemble} object.
#' @param field.ref
#'      True (synthetic) field.
#' @param \dots additional parameters
#' @param par.settings settings
#'
#' @export
plot.field.ensemble <- function(
    x,
    field.ref = NULL,
    xlab = 'X',
    ylab = 'Y',
    ...,
    par.settings = list()
    )
{
    par.settings <- utils::modifyList(
        getOptions('grid.par.settings'),
        par.settings)

    ndim <- if (is.null(dim(x$fields[[1]]))) 1 else
                length(dim(x$fields[[1]]))

    nfields <- length(x$fields)
    field.len <- length(x$fields[[1]])

    grid <- x$grid
    if (is.null(grid))
    {
        if (ndim == 1L)
            grid <- list(from = .5, by = 1, len = field.len)
        else
            grid <- list(from = rep(.5, ndim), by = rep(1, ndim),
                len = dim(x$fields[[1]]))
    }

    field.xyz <- grid.fullxyz(grid)

    if (ndim == 1)
    {
        yy <- unlist(x$fields)
        xx <- rep(c(field.xyz), nfields)

        groups <- rep(seq_len(nfields), each = field.len)

        xyplot(
            yy ~ xx,
            prepanel = function(x, y, ref.y = NULL, ...)
                {
                    ylim <- quantile(y, c(.02, .98))
                    if (!is.null(ref.y))
                    {
                        ylim[1] <- min(ylim[1], min(ref.y))
                        ylim[2] <- max(ylim[2], max(ref.y))
                    }
                    list(ylim = ylim)
                },
            panel = function(..., ref.x, ref.y = NULL,
                ref.col = 'black', ref.lty = 'solid', ref.lwd = 3)
                {
                    if (!is.null(ref.y))
                        llines(x = ref.x, y = ref.y, col = ref.col,
                            lty = ref.lty, lwd = ref.lwd)
                    panel.xyplot(...)
                },
            ref.x = c(field.xyz),
            ref.y = field.ref,
            groups = groups,
            type = 'l',
            lty = 'solid',
            xlab = xlab,
            ylab = ylab,
            par.settings = par.settings,
            ...
            )
    } else if (ndim == 2)
    {
        zz <- unlist(x$fields)
        xx <- rep(field.xyz[, 1], nfields)
        yy <- rep(field.xyz[, 2], nfields)

        panel.names <- paste(seq_len(nfields))

        if (!is.null(field.ref))
        {
            zz <- c(c(field.ref), zz)
            xx <- c(field.xyz[, 1], xx)
            yy <- c(field.xyz[, 2], yy)
            panel.names <- c('reference', panel.names)
        }

        panel.names <- factor(panel.names,
            levels = panel.names, ordered = TRUE)

            # 'col.regions' and 'at' parameters
            # are in 'panel.args.common' of the lattice plot.
            # They can be changed by manipulating the lattice object.
            # That's why we are not making extradodordinary
            # efforts to set them here.

        cuts <- 49
        dd <- range(zz)
        col.at <- seq(dd[1], dd[2], length = cuts)

        if (length(panel.names) == 1L)
        {
            levelplot(
                zz ~ xx * yy,
                aspect = 'iso',
                col.regions = terrain.colors(cuts + 1),
                    # col.regions = rainbow(cuts + 1, start = 4/6),
                cuts = cuts,
                at = col.at,
                strip = FALSE,
                par.settings = par.settings,
                xlab = xlab,
                ylab = ylab,
                ...
                )
        } else
        {
            levelplot(zz ~ xx * yy | rep(panel.names, each = field.len),
                aspect = 'iso',
                col.regions = terrain.colors(cuts + 1),
                cuts = cuts,
                at = col.at,
                strip = TRUE,
                par.settings = par.settings,
                xlab = xlab,
                ylab = ylab,
                ...
                )
        }

    } else
        stop('3D plotting not implemented')
}




#' Summarize an ensemble of field realizations.
#'
#' @param object
#'      A 'field.ensemble' object.
#' @param field.ref
#'      True (synthetic) field.
#'      If present, the \code{mad} element of the output is the mean
#'      absolute deviation of the simulations from this reference field.
#'      If absent, the 'mad' element is the mean absolute deviation
#'      of the simulations from the median of the simulations.
#' @param q
#'      Quantiles to be obtained as summary.
#'
#' @return
#'      A list of class \code{summary.field.ensemble},
#'      which is the 'object' plus the following new elements:
#'      \describe{
#'          \item{\code{mean}}{point-wise mean of the fields}
#'          \item{\code{median}}{point-wise median of the fields}
#'          \item{\code{sd}}{point-wise standard deviation of the fields}
#'          \item{\code{mad}}{mean absolute deviation of the fields from
#'              \code{field.ref} if available, or from
#'              the median of the realizations otherwise}
#'          \item{\code{q}}{point-wise quantiels of the fields}
#'          \item{\code{ref}}{\code{field.ref}}
#'      }
#'
#' @export
summary.field.ensemble <- function(
    object,
    ...,
    field.ref = NULL,
    q = c(0.05, 0.25, 0.5, 0.75, 0.95)
    )
{
    field.dim <- dim(object$fields[[1]])
    if (is.null(field.dim))
        field.dim <- length(object$fields[[1]])


    fields <- do.call(rbind, lapply(object$fields, c))
        # Each row is a field.
        # Each col is a location.

    fields.mean <- .colMeans(fields, nrow(fields), ncol(fields), TRUE)
    fields.q <- apply(fields, 2, quantile, q, na.rm = TRUE)
        # Each row is a quantile.
        # Each col is a location.
    fields.sd <- apply(fields, 2, sd, na.rm = TRUE)

    idx <- which(abs(q - .5) < .Machine$double.eps^0.5)
    if (length(idx))
        fields.median <- fields.q[idx, ]
    else
        fields.median <- apply(fields, 2, median, na.rm = TRUE)

    if (is.null(field.ref))
        fields.mad <- apply(fields, 2, mad, na.rm = TRUE)
    else
        fields.mad <- apply(t(fields) - c(field.ref), 1, mad,
            center = 0, na.rm = TRUE)

    if (length(field.dim) > 1)
    {
        dim(fields.mean) <- field.dim
        dim(fields.sd) <- field.dim
        dim(fields.median) <- field.dim
        dim(fields.mad) <- field.dim
    }

    fields.q <- t(fields.q)
    dim(fields.q) <- c(field.dim, length(q))
        # First 1 or 2 dimensions are a quantile field.
        # Each quantile takes one 2nd or 3rd dimension.

    object$mean <- fields.mean
    object$median <- fields.median
    object$sd <- fields.sd
    object$mad <- fields.mad
    object$q <- fields.q
    object$ref <- field.ref

    structure(object, class = 'summary.field.ensemble')
}



#' Plot a \code{summary.field.ensemble} object.
#'
#' @param x
#'      An \code{summary.field.ensemble} object.
#' @param \dots additional parameters
#' @param par.settings settings
#'
#' @export
plot.summary.field.ensemble <- function(
    x,
    ...,
    par.settings = list()
    )
{
    par.settings <- utils::modifyList(
        getOptions('grid.par.settings'),
        par.settings)

    ndim <- if (is.null(dim(x$mean))) 1 else length(dim(x$mean))

    grid <- x$grid
    if (is.null(grid))
    {
        if (is.null(dim(x$mean)))
            grid <- list(from = .5, by = 1, len = length(x$mean))
        else
            grid <- list(from = rep(.5, ndim), by = rep(1, ndim),
                len = dim(x$mean))
    }


    field.xyz <- grid.fullxyz(grid)

    if (ndim == 1)
    {
        yy <- c(x$q)
        xx <- rep(c(field.xyz), ncol(x$q))

        groups <- c(col(x$q))

        xyplot(
            yy ~ xx,
            prepanel = function(x, y, ref.y = NULL, ...)
                {
                    ylim <- quantile(y, c(.02, .98))
                    if (!is.null(ref.y))
                    {
                        ylim[1] <- min(ylim[1], min(ref.y))
                        ylim[2] <- max(ylim[2], max(ref.y))
                    }
                    list(ylim = ylim)
                },
            panel = function(..., ref.x, ref.y = NULL,
                ref.col = 'red', ref.lty = 'solid', ref.lwd = 2)
                {
                    if (!is.null(ref.y))
                        llines(x = ref.x, y = ref.y, col = ref.col,
                            lty = ref.lty, lwd = ref.lwd)
                    panel.xyplot(...)
                },
            ref.x = c(field.xyz),
            ref.y = x$ref,
            groups = groups,
            type = 'l',
            lty = 'solid',
            col = 'black',
            xlab = 'x', ylab = 'y',
            par.settings = par.settings,
            ...
            )
    } else if (ndim == 2)
    {
        myplots <- list()

            # 'col.regions' and 'at' parameters
            # are in 'panel.args.common' of the lattice plot.
            # They can be changed by manipulating the lattice object.
            # That's why we are not making extradodordinary
            # efforts to set them here.

        cuts <- 49
        myplots$medianplot <- levelplot(
            c(x$median) ~ field.xyz[, 1] * field.xyz[, 2],
            aspect = 'iso',
            col.regions = terrain.colors(cuts + 1),
                # col.regions = rainbow(cuts + 1, start = 4/6),
            cuts = cuts,
            par.settings = par.settings,
            main = 'median',
            ...
            )

        if (!is.null(x$ref))
        {
            panel.names <- c('reference', 'median')
            panel.names <- factor(panel.names,
                levels = panel.names, ordered = TRUE)
            panel.names <- rep(panel.names, each = length(x$ref))

            dd <- range(c(x$ref, x$median))
            col.at <- seq(dd[1], dd[2], length = cuts)

            myplots$medianrefplot <- levelplot(
                c(x$ref, x$median)
                    ~ rep(field.xyz[, 1], 2) * rep(field.xyz[, 2], 2)
                    | panel.names,
                col.regions = terrain.colors(cuts + 1),
                cuts = cuts,
                at = col.at,
                strip = TRUE,
                aspect = 'iso',
                par.settings = par.settings,
                main = 'median vs reference',
                ...
                )
        }

        # Simulation standard error at each location.
        myplots$sdplot <- levelplot(
            c(x$sd) ~ field.xyz[, 1] * field.xyz[, 2],
            aspect = 'iso',
            col.regions = rev(gray.colors(cuts + 1)),
            cuts = cuts,
            par.settings = par.settings,
            main = 'sd',
            ...
            )

        # Simulation mean absolute deviation (from truth or from
        # median of simulations) at each location.
        myplots$madplot <- levelplot(
            c(x$mad) ~ field.xyz[, 1] * field.xyz[, 2],
            aspect = 'iso',
            col.regions = rev(gray.colors(cuts + 1)),
            cuts = cuts,
            par.settings = par.settings,
            main = 'mad',
            ...
            )

        myplots
    } else
        stop('3D plotting not implemented')
}

