sample.by.design <- function(
    ...
    )
# There can be as many arguments as needed; they are interpreted
# the same way.
#
# Each argument is a list with 3 elements:
#   list(divide, n, jitter)
# 'divide', 'n', and 'jitter' are used to help explanations
# here; the argument list is treated as having unnamed
# members, i.e. the index of these three elements are assumed.
#
# 'divide' is an integer vector with as many elements as
# there are dimensions; it specifies a division of the
# space.
# For example, c(3, 4) means 'x' is divided evenly into 3 intervals
# and 'y' is divided evenly into 4 intervals. Therefore there
# 3 by 4 cells in total.
#
# 'n' is an integer matrix or vector indicating number of samples in
# each cell. If matrix, its 'dim' must be equal to 'divide';
# if vector, its 'length' must be either 1 or equal to the total number
# of cells, i.e. prod(divide).
# If matrix, it is read in column order as if it were a vector.
# This provides the flexibility of have multiple samples in each cell;
# it would not be surprising if one just provides
# a single number, say 1 or 2.
#
# 'jitter' is a matrix or vector, patterns the same way as 'n'.
# It's a number on [0, 1], indicating the wiggle room of the samples
# in each cell.
# If 0, the sample is at the cell center;
# if 1, anywhere uniformly in the cell.
#
# E.X.: for 10 samples uniformly in the entire 2-D space:
#  list(c(1,1), 10, 1)
#
# The return is a matrix of coordinates, each row being one sample
# location. The coordiate in each dimension is on [0, 1].
{
    layout <- list(...)
    if (length(layout) > 1)
        stopifnot(length(unique(
            sapply(lapply(layout, '[[', 1), length)
            )) == 1)
            # Dimensions should be consistent.

    xyz <- c()
    ndim <- length(layout[[1]][[1]])

    for (i.layout in seq_along(layout))
    {
        divide <- layout[[i.layout]][[1]]
        intervals <- 1 / divide

        n <- layout[[i.layout]][[2]]
        if (length(n) == 1)
            n <- rep(n, prod(divide))
        else
            stopifnot(length(n) == prod(divide))

        jitter <- layout[[i.layout]][[3]]
        stopifnot(jitter >= 0, jitter <= 1)
        if (length(jitter) == 1)
            jitter <- rep(jitter, prod(divide))
        else
            stopifnot(length(jitter) == prod(divide))

        ind <- as.matrix(expand.grid(
            lapply(divide, function(n) 1 : n)))

        for (i.row in 1 : nrow(ind))
        {
            if (n[i.row] < 1) next
            ctr <- (ind[i.row, ] - 0.5) * intervals
            for (j in 1 : n[i.row])
            {
                xyz <- rbind(xyz,
                    ctr + runif(ndim, -jitter[i.row], jitter[i.row]) *
                        intervals/2)
            }
        }
    }


    xyz
        # With a model grid defined by 'flexgrid',
        # 'xyz' is converted to the model grid coordinates by
        #
        #   n <- nrow(xyz)
        #   xyz <-
        #       rep(grid$start - grid$step/2, each = n) +
        #           xyz * rep(grid$step * grid$size, each = n)
        #
        # One may want to 'snap' the sample locations
        # to the grid points. This can be achieved by
        #
        #  grid.ijk2xyz(grid.xyz2ijk(grid, xyz))
        # where 'xyz' has been converted as above.
}

