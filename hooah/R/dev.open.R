dev.open <-
function(device=ifelse(is.null(file), getOption('device'), 'postscript'),
        file = NULL,
        paper = NULL,
            # Character 'letter' or 'beamer',
            # or length-two vector c(width, height) in inches.
        wid=.75, hgt=.75,
            # Figure size as fraction of paper size.
        width, height,
            # 'inches' for 'postscript' and 'pdf'; 'pixels' for 'jpeg' and 'png'.
            # Better use 'wid' and 'hgt' for 'jpeg' and 'png' and let me
            # calculate the size in terms of pixels.
        asp,
            # height/width
        res = 72,
            # Used with 'jpeg' and 'png'; don't use other values.
        ...)
{
# If 'file' is NULL, then 'device' should be an 'interactive' device.
# If 'device' is omitted,
# the default (interactive) device (say 'X11') will be opened.
# One could as well specify 'X11'.
#
# If 'file' is not NULL, then 'device' should be one of those
# devices for saving files, e.g. 'postscript', 'pdf', 'jpeg', 'png'.
#
# 'paper' implies the full size of the target use.
# If 'file' is not NULL, 'paper' is the physical paper,
# e.g. 'letter', 'a4'.
# The 'beamer' size is for making presentations with the Beamer LaTeX
# class.
# For custom-sized paper, pass 'paper' as a length-two vector with
# c(width, height) in inches.
# If 'file' is NULL and 'paper' is also NULL, the role of 'paper'
# is played by the full size of the interactive device being used.
# However, one can just as well specify a 'paper' to make it
# indicate the full size of the device.
#
# There are two ways to specify the actual size of the plot on 'paper'.
# (1) 'wid' and 'hgt' specify fractions of 'paper'.
# (2) 'width' and 'height' specify the size directly (in inches),
# ignoring 'paper'.
#
# 'asp' is the desired aspect ratio of the figure:
#   height / width
# If one of wid(width) and hgt(height) is missing, the other is
# calculated using 'asp'.
# If both wid(width) and hgt(height) are specified, then
# the figure is put within the 'width' by 'height'
# rectangle but respect the required 'asp'; this usually means shrinking
# the figure somehow while keeping the aspect ratio to 'asp'.
#
# ...: Arguments to be passed to the device function.
#
# About font size and line width:
#
# Device functions (like x11() and postscript()) have argument
# 'pointsize'. If set, par('ps') will refect this setting.
# This setting will also be picked up by GRID to be the base font size
# used in plotting.
#
# If font size is set by par(ps = x) AFTER the device has been opened,
# then this setting WILL NOT be picked up by GRID.
#
# Whatever the font size setting for the device, LATTICE will use ITS
# OWN default sizes.
#
# GRID function 'gpar' only creates 'gpar' objects and does not actually
# SET anything. GRID settings must be passed into function calls through
# 'gp = gpar(...)'.
#
# Device functions do not take argument 'lwd', therefore one can not
# globally set line width there. 'par' takes argument 'lwd'; GRID takes
# arguments 'lwd' and 'lex'.
#
# This function sets fontsize (pointsize) and linewidth based on the
# device. If subsequently lattice plots are drawn unto this device,
# lattice uses its own default settings.
# However, the overall font size and line width can be scaled by
#
#  xyplot(...,
#         par.settings = list(
#              grid.pars = list(cex = par('ps') * par('cex') / 12,
#                               lex = par('lwd')))
#        )
#
# LATTICE assumes an open postscript device is b/w. In order to make color
# plots, use
#
#   trellis.device(new = FALSE, color = TRUE)
#
# following the call to the current function.


    is.for <- ifelse(is.null(file), '', 'print')

    if (is.character(device))
    {
        device.name <- device
        device <- get(device.name)
        if (!is.function(device))
            stop("unknown device")
    }
    else
        stop("'device' must be a string of the device name")
        # Passing the device function, instead of the function's name,
        # is no problem for function calling, but the real name of that
        # function will be hidden (in this function it is only known as
        # 'device'), therefore I'll be unable to tell the device type
        # and do some other settings accordngly.

    if (is.null(file) &&
        !(tolower(device.name) %in%
            tolower(c('X11', 'windows', 'quartz', 'GTK', 'gnome', 'JaveGD'))))
        stop('need a file to which to save the plot on non-interactive device')


    # Verify file name extension.
    if (!is.null(file))
    {
        ext <- c('eps', 'pdf', 'jpg', 'png')[match(device.name,
                c('postscript', 'pdf', 'jpeg', 'png'), nomatch = NA)]
        if (!is.na(ext))
        {
            if (!any(grep(paste('[.]', ext, '$', sep = ''), file,
                ignore.case = TRUE)) &&
                (!(device.name == 'postscript' &&
                    any(grep('[.]ps$', file, ignore.case = TRUE)))))
                file <- paste(file, '.', ext, sep = '')
        }
    }

    if (missing(width) || missing(height))
    {
        if (is.null(paper))
        {
            if (is.null(file))
                # If plot is not to be saved to a file,
                # the screen is the 'paper'.
                paper <- dev.screen.info()$size.mm / 25.4  # inches
            else
                paper <- getOption('papersize')
        }
        if (is.character(paper))
        {
            if (paper == 'letter') paper <- c(6.5, 9) # leave 1 in margins.
            else
                if (paper == 'beamer')
                {
                    paper <- c(5.04, 3.78)
                        # The Beamer paper size is 128 by 96 mm (5.04 by 3.78 in).
                    is.for <- 'talk'
                }
        }
        if (!is.numeric(paper) || length(paper) != 2)
            stop("unknown value for 'paper'")

        if (missing(width))
        {
            width <- paper[1] * wid
            if (device.name %in% c('jpeg', 'png'))
                width <- width * res
        }
        if (missing(height))
        {
            height <- paper[2] * hgt
            if (device.name %in% c('jpeg', 'png'))
                height <- height * res
        }
    }

    if (!missing(asp))
    # Put the plot within the specified frame of width and height,
    # keeping the aspect ratio.
    {
        height <- min(height, width * asp)
        width <- min(width, height / asp)
    }


    args <- list()
    pars <- list()

    args$width <- width
    args$height <- height
    if (!is.null(file)) args$file <- file

    if (is.for == 'print')
    {
        args$pointsize <- 10
        pars$lwd <- 1
    }
    else
        if (is.for == 'talk')
        {
            args$pointsize <- 12
            pars$lwd <- 2
        }

    if (device.name %in% c('jpeg', 'png'))
        args$res <- res

    dots <- list(...)
    args[names(dots)] <- dots
        # If the user has specified something (say, 'pointsize'),
        # then it will overwrite my setting.

    do.call(device, args)

    # Additional settings according to general, personal preference.
    # There is no need to pass in other specific settings for 'par'
    # because if one knows what to set, he may call 'par' himself after
    # 'dev.open'.

    pars$tcl = -.35
    pars$las = 1
        # Make all axis lables horizontal for easy reading. This is
        # Trellis style.

    par(pars)
    invisible(NULL)
}



dev.screen.info <-
function()
# Call 'xdpyinfo' (display information utility for X) to get
# a vareity of information about the monitor, and return
# a list containing most of the info of interest to the programmer.
# If other info is ever need, read the man page of 'xdpyinfo'
# and expand the returned list of this function.
# This version works when there is only one monitor.
#
# Command 'xdpyinfo' is not always available.
# So use a 'try' function.
{
    display.size <- try(system(
        "xdpyinfo | grep dimensions | tr -d '[a-wy-z: ()]'",
        intern = TRUE))
      # The 'dimensions' line returned by 'xdpyinfo' typically
      # looks like
      #   dimensions:    1024x768 pixels (342x271 millimeters)
      # The above returns 4 numbers separated by 3 'x's.
    if (inherits(display.size, 'try-error'))
        display.size <- '1024x768x342x271'
    display.size <- as.numeric(unlist(
        strsplit(display.size, split = 'x')))

    display.dpi <- try(system(
        "xdpyinfo | grep resolution | tr -d '[a-wy-z: ()]'",
        intern = TRUE))
      # The 'resolution' line returned by 'xdpyinfo' typically
      # looks like
      #   resolution:    76x72 dots per inch
      # The above returns 2 numbers separated by a 'x'.
    if (inherits(display.dpi, 'try-error'))
        display.dpi <- '76x72'
    display.dpi <- as.numeric(unlist(
        strsplit(display.dpi, split = 'x')))

    list(size.pixel = display.size[1:2],
         size.mm = display.size[3:4],
         dpi = display.dpi)
}



