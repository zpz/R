# 'minus zero plus colors' generates a color palette
# of length 'n'
# that is dark blue at the negative end and dark red at the positive
# end, and is about white around 0.
# Such a palette is useful when on a map it is desirable to easily
# diffrentiate the negative and the positive regions.
#
# See also: rainbow.
mzp.colors <-
function(n = 100, range = c(-1, 1))
# 'range' gives the relative location of '0' in the value range.
# By default '0' is at the midpoint.
# The absolute values of 'range' does not matter.
{
    if ((n %% 1) != 0 || n <= 1)
        stop("'n' must be an integer greater than 1.")

    x <- max(abs(range))
    h <- approx(x = c(-x, 0, 1e-10, x),
        y = c(4/6, 3/6 - 1/20, 1/6 + 1/20, 0),
        xout = seq(range[1], range[2], length.out = n))$y
    #h <- seq(4/6, 0, length.out = n)
    s <- approx(x = c(-x, 0, x), y = c(0, 1, 0),
        xout = seq(range[1], range[2], length.out = n))$y
    s <- 1 - s*s
    hsv(h = h, s = s)
}
