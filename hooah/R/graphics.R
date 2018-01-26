#' 'unlist' a hierarchical structure composed of lattice plots.
#'
#' The returned object can then be iterated and printed,
#' that is, plotted.
#' Suppose the output is 'z', then it may be used this way:
#'   for (x in z) { x11(); print(x) }
#'
#' @export
unpack.lattice.plots <- function(x)
{
    if (is.list(x))
        if ('trellis' %in% class(x))
            # 'x' is not a list containing a trellis object,
            # but just a trellis object?
            list(x)
        else
            do.call(c, lapply(x, unpack.lattice.plots))
    else
        invisible(NULL)
}



#' Default panel function for \code{bwstatsplot}
#'
#' @export
panel.bwstatsplot <- function(
    x, y,
        # These are ignored.
    horizontal,
    box.width,
    ...,
    x.at,
    stats)
{
    if (is.list(x.at))
    {
        x.at <- x.at[[lattice::panel.number()]]
        stats <- stats[[lattice::panel.number()]]
    }
    stats <- lapply(
        stats,
        function(x) unlist(x[c('n', 'stats', 'conf', 'out')])
        )

    if (horizontal)
    {
        y <- rep(x.at, sapply(stats, length))
        x <- unlist(stats)
    } else
    {
        x <- rep(x.at, sapply(stats, length))
        y <- unlist(stats)
    }


    f.stats <- function(x, coef, do.out)
    {
        # 'coef' is ignored.
        # Should have been used while creating 'stats'.
        # Can't be changed now.
        z <- list(
            n = x[1],
            stats = x[2:6],
            conf = x[7:8]
            )
        if (do.out && length(x) > 8L)
            z$out <- x[-(1:8)]
        else
            z$out <- numeric(0)
        z
    }

    if (missing(box.width))
        box.width <- diff(range(x.at)) /
            min(25, length(x.at)) / 2
        # FIXME:
        # get xlim of current panel and determine
        # 'box.width' based on that;
        # I don't remember the function for now.

    lattice::panel.bwplot(
        x = x, y = y,
        horizontal = horizontal,
        box.width = box.width,
        ...,
        stats = f.stats,
    )
}


#' Box plots given output of the function \code{boxplot.stats}
#' instead of the actual data.
#'
#' @param stats
#'      List containing outputs of \code{boxplot.stats}.
#'      If each member of \code{stats} contains the output of
#'      a call to \code{boxplot.stats}, then the plot
#'      will have one panel.
#'      If each member of \code{stats} is a list that contains
#'      a list of outputs of \code{boxplot.stats},
#'      then the plot will have multiple panels,
#'      each corresponding to one element of \code{stats}.
#' @param x.at
#'      Location of the boxes on
#'      the horizontal (if \code{horizontal} is \code{FALSE})
#'      or vertical (if \code{horizontal} is \code{TRUE}) axis.
#'      If missing, serial numbers will be used.
#' @param panel panel function.
#' @param strip strip
#' @param horizontal orientation
#' @param par.settings settings
#' @param \dots additional parameters
#'
#' @export
bwstatsplot <- function(
    stats,
    x.at = NULL,
    panel = panel.bwstatsplot,
    strip,
    horizontal = FALSE,
    par.settings = list(),
    ...
    )
{
    par.settings <- utils::modifyList(
        list(plot.symbol = list(pch = 3, cex = .5, col = 'black'),
                # Controlling outliers.
            box.rectangle = list(col = 'black'),
                # Controlling the box.
            box.umbrella = list(col = 'black')
                # Controlling the whiskers.
            ),
        par.settings)

    if (all(c('stats', 'n', 'conf') %in% names(stats[[1]])))
    {
        n.panels <- 1L

        if (is.null(x.at))
        {
            x.at <- seq_along(stats)
        } else
            stopifnot(length(x.at) == length(stats))

        # Dummy points to help 'prepanel' set the range.
        y <- lapply(stats, function(x) c(x$stats, x$out))
        x <- rep(x.at, sapply(y, length))
        y <- unlist(y)

        if (horizontal)
            z <- formula(x ~ y)
        else
            z <- formula(y ~ x)
    } else
    {
        n.panels <- length(stats)

        # Panel titles.
        pp <- names(stats)
        if (is.null(pp))
            pp <- as.character(seq_along(stats))

        if (is.null(x.at))
        {
            x.at <- lapply(stats, seq_along)
        } else
        {
            if (!is.list(x.at))
                x.at <- rep(list(x.at), length(stats))
            stopifnot(length(x.at) == length(stats),
                sapply(stats, length) == sapply(x.at, length)
                )
        }

        # Dummy points to help 'prepanel' set the range.
        y <- lapply(stats, lapply, function(x) c(x$stats, x$out))
        x <- lapply(seq_along(y), function(i)
                rep(x.at[[i]], sapply(y[[i]], length)))
        cell <- rep(seq_along(stats), sapply(x, length))
        x <- unlist(x)
        y <- unlist(y)

        cell <- factor(cell, levels = seq_along(stats), ordered = TRUE,
            labels = pp)

        if (horizontal)
            z <- formula(x ~ y | cell)
        else
            z <- formula(y ~ x | cell)
    }

    lattice::xyplot(
        z,
        panel = panel,
        pch = '|',
        cex = 0.5,
        col = 'black',
            # Symbol, size, and color of the median dot.
        horizontal = horizontal,
        strip = if (missing(strip)) (n.panels > 1L) else strip,
        par.settings = par.settings,
        x.at = x.at,
        stats = stats,
        ...
        )
}



find.x.at <- function(data, x.at = 'data')
{
    if (is.character(x.at))
    {
        x.at <- match.arg(x.at, c('data', 'serial'))
            if (x.at == 'data')
                x.at <- data
            else
                x.at <- seq_along(data)
    } else
    {
        stopifnot(is.numeric(x.at),
            length(x.at) == length(data))
        x.at <- c(x.at)
    }
    x.at
}



#' Plot the data reproduction in each iteration.
#'
#' Use one panel for each iteration;
#' in each panel, sample for each datum is
#' a vertical boxplot horizontally located
#' where the observed value is.
#' Because there can be many iterations,
#' a selection of iterations are used for the plot.
#'
#' @param x an \code{anchorit} object.
#' @param \dots etc
#' @param data.forward.groups
#'      factor vector indicating group membership
#'      of each element of the forward data,
#'      used when the forward data are a composite of
#'      multiple data sets that should be
#'      separated in graphics because of disparate maganitude levels.
#'
#' @return
#'      If \code{data.forward.groups} is \code{NULL},
#'      a \pkg{lattice} object; otherwise,
#'      a list of \pkg{lattice} objects, one for each group of
#'      forward data.
#'
#' @export plot.forward.reproduction
#' @usage plot.forward.reproduction(...)
plot.forward.reproduction <- function(
    forward.data,
    forward.sample,
    x.at,
    data.forward.groups = NULL,
    xlab = 'Observation',
    ylab = 'Simulation',
    main = 'Data reproduction',
    par.settings = list(),
    ...
    )
{
    par.settings <- utils::modifyList(
        getOptions('grid.par.settings'),
        par.settings)

    if (missing(x.at))
        aspect <- 'iso'
    else
        aspect <- 'fill'


    my.panel <- function(..., ref.x, ref.y,
        ref.col = 'red', ref.lty = 'solid', ref.lwd = 1)
    {
        panel.bwstatsplot(...)
        if (is.list(ref.x))
            ref.x <- ref.x[[lattice::panel.number()]]
        if (is.list(ref.y))
            ref.y <- ref.y[[lattice::panel.number()]]
        if (all(ref.x == ref.y))
            lattice::panel.abline(0, 1, col = ref.col,
                lty = ref.lty, lwd = ref.lwd)
        else
            lattice::llines(ref.x, ref.y, col = ref.col,
                lty = ref.lty, lwd = ref.lwd)
    }


    if (!is.null(data.forward.groups))
    {
        # FIXME
        # does not allow general cases.
        if (is.list(data.forward.groups))
            stop('not implemented')
        if (!is.factor(data.forward.groups))
            data.forward.groups <- as.factor(data.forward.groups)
        if (length(levels(data.forward.groups)) <= 1L)
            data.forward.groups <- NULL
    }


    n.iter <- length(forward.sample)

    if (is.null(data.forward.groups))
    {
        allout <- unlist(lapply(forward.sample,lapply, '[[', 'out'))
        if (diff(range(allout)) > diff(range(forward.data)) * 10)
            scales <- 'free'
        else
            scales <- list()

        if (missing(x.at))
            x.at <- lapply(rep(list(forward.data), n.iter), find.x.at)
        else
        {
            if (!is.list(x.at))
                x.at <- rep(list(x.at), n.iter)
            x.at <- lapply(seq_along(x.at), function(i)
                find.x.at(forward.data, x.at[[i]]))
        }

        ref.x <- lapply(x.at, sort)
        ref.y <- lapply(seq_along(x.at),
            function(idx) forward.data[order(x.at[[idx]])])

        box.width <- min(sapply(x.at, function(x)
            diff(range(x)) / min(25, length(x)) / 2))

        bwstatsplot(
            structure(forward.sample,
                names = paste('iteration', 1:n.iter)),
            x.at = x.at,
            aspect = aspect,
            scales = scales,
            box.width = box.width,
            panel = my.panel,
            ref.x = ref.x,
            ref.y = ref.y,
            par.settings = par.settings,
            xlab = xlab,
            ylab = ylab,
            main = main,
            ...
            )
    } else
    {
        n.forward <- length(forward.data)
            # For now, forward data in all iterations must be the same.

        stopifnot(length(data.forward.groups) == length(forward.data))

        n.grp <- length(levels(data.forward.groups))
        myplots <- vector('list', n.grp)

        if (missing(x.at))
            x.at <- rep(list('data'), n.grp)
        else if (is.character(x.at))
            x.at <- as.list(rep(x.at, length.out = n.grp))
        else
            stopifnot(is.list(x.at), length(x.at) == n.grp)

        for (i.grp in seq_len(n.grp))
        {
            data.idx <- which(data.forward.groups ==
                levels(data.forward.groups)[i.grp])
            my.data <- forward.data[data.idx]
            my.sample <- lapply(forward.sample, '[', data.idx)
            my.lab <- as.character(levels(data.forward.groups)[i.grp])

            if (length(data.idx) == 1L)
            {
                warning('not implemented')
                    # FIXME: use a comparative box plot.
                myplots[[i.grp]] <- list(NULL)
            } else
            {
                my.x.at <- find.x.at(my.data, x.at[[i.grp]])

                allout <- unlist(lapply(my.sample, lapply, '[[', 'out'))
                if (diff(range(allout)) > diff(range(my.data)) * 10)
                    scales <- 'free'
                else
                    scales <- list()

                myplots[[i.grp]] <- bwstatsplot(
                    structure(my.sample, names = paste('iteration', 1:n.iter)),
                    x.at = my.x.at,
                    aspect = aspect,
                    scales = scales,
                    box.width = diff(range(my.x.at))
                        / min(25, length(my.x.at)) / 2,
                    panel = my.panel,
                    ref.x = sort(my.x.at),
                    ref.y = my.data[order(my.x.at)],
                    par.settings = par.settings,
                    xlab = xlab,
                    ylab = my.lab,
                    main = paste(main, my.lab),
                    ...
                    )
            }
        }

        myplots
    }
}


