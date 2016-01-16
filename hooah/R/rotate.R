rotate <-
function(x, y = NULL, angle = 0) {
# Rotate points on the Cartesian plane by a angle.
#
# Rotate v: (x, y) by angle with sine and cosine:
#   (x, y) --> (x * cosine - y * sine, y * cosine + x * sine)
#
# The angle can be one single value applied to the entire vector X,
# or can be a vector corresponding to each element of X.
# If angle is complex number(s), it is point(s) indicating the direction.
# If angle is real number(s), it is angle value in radian.

    xy <- xy.coords(x, y)

    if (is.complex(angle)) {
        dist <- abs(angle)
        cosx <- Re(angle) / dist
        sinx <- Im(angle) / dist
    } else {
        cosx <- cos(angle)
        sinx <- sin(angle)
    }

    xx <- xy$x * cosx - xy$y * sinx
    yy <- xy$x * sinx + xy$y * cosx

    list(x = xx, y = yy)
}

