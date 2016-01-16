par.uin <-
function() {
#  Inches per axes unit in X and Y.
#  Same as S function par("uin").

    u <- par("usr")
    p <- par("pin")
    c(p[1]/(u[2] - u[1]), p[2]/(u[4] - u[3]))
}

