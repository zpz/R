# A wrapper for 'aggregate' that guards against the case where
# the data size is too big.
robust.aggregate <- function(
    x,
    by,
    FUN
    )
{
    if (length(x) <= 5e6)
    {
        z <- aggregate(x, by, FUN)
    } else
    {
        if (!is.list(by))
            stop("'by' must be a list")

        uby <- unique(as.data.frame(by))
        uby <- uby[do.call(order, rev(as.list(uby))), , drop = FALSE]
            # 'rev' the ordering of the list elements to
            # make the first col steps the fastest, the second col
            # next, and so on.

        res <- vector(mode(x), nrow(uby))

        for (i in 1 : length(res))
        {
            res[i] <- do.call(FUN,
                list(
                    x[ apply(
                        sapply(
                            1 : length(by),
                            function(j) by[[j]] == uby[[j]][i]
                            ),
                        1,
                        all) ]
                    )
                )
        }

        z <- cbind(uby, x = res)
    }
    z
}
