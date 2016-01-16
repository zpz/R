trim <-
function(x) {
# Trim off leading and trailing spaces in a string.
    sub(" *([^ ]+.*[^ ]) *", "\\1", x)
}

