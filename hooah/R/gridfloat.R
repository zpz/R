# Read in the data matrix in a GridFloat elevation data file.
# The data are stored in row-major order.
# The accompanying header file contains numbers of rows, columns, and
# other info.
# Such elevantion data are rich resources for testing models of 1-D and
# 2-D random fields.
# The National Elevation Dataset (NED) can be downloaded from
# the National Map Seamless Server operated by USGS.
# URL: http://seamless.usgs.gov

gridfloat <- function(file, hdr = sub('.flt$', '.hdr', file))
{
    x <- read.table(file = hdr)
    ncols <- as.numeric(as.character(x[[2]][x[[1]] == 'ncols']))
    nrows <- as.numeric(as.character(x[[2]][x[[1]] == 'nrows']))
    endian <- as.character(x[[2]][x[[1]] == 'byteorder'])
    endian <- ifelse(endian == 'LSBFIRST', 'little', 'big')

    data <- readBin(file, what = 'double', n = ncols * nrows, size = 4,
        endian = endian)
    data <- matrix(data, ncol = ncols, byrow = TRUE)
        # This matrix, if printed, is analogous to the shown map:
        #  first row is the top row of the map,
        #  second row is the second row from top of the map,
        # and so on.
    data <- t(data[nrows : 1, ])
        # Row number is proportional to 'X' coord (west -> east);
        # col number is proportional to 'Y' coord (south -> north).
        # A direct printout of this matrix is not intuitive,
        # but a plot by
        #   images(z = data, ...)
        # is the correct picture.
    data
}

