# This function shows the pre-defined colors
# to help choose colors in plotting.
# One usually needs to drag the created window horizontally
# to make it several times larger to see the details.
colors.show <-
function()
{
    col <- colors()
    n <- length(col)

    y <- 1 : 20
    x <- 1 : ceiling(n / max(y))
    z <- matrix(1 : (length(x) * length(y)),
        nrow = length(x), ncol = length(y))
    z <- (z - 1) %% n + 1
    x11()
    image(x = x, y = y, z = z, col = col)
    text(x = rep(x, length(y)), y = rep(y, each=length(x)),
        labels = col[z], col = 255 - col2rgb(col[z]),
        adj = c(.5, .5), cex = .85)
}
