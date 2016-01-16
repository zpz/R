xdr <-
function(file, action = 'r') {

    if (action != 'r') {
        stop ('for the present parameter "action" accepts value "r" only')
    }

    fin <- file(file, paste(action, 'b', sep = ''))
        # File opened.
        # Rememer to close with with xdr.close.

    info <- file.info(file)
    size <- unname(unlist(info['size']))

    x <- list(file = file, fid = fin,
        size = size,
        endian = ifelse(.Platform$endian == 'big', 'big', 'swap'))
    class(x) <- 'xdr'
    x
}


# Read in an array of numbers that are stored contiguously
# and are of the same type.
xdr.readArray <-
function(obj, addr, type, n) {
# 'obj' is of class 'xdr'.
# 'addr' is the starting address of the requested data in the file.
#   The very begining of the file has 'addr' value 0.
# 'type' is the type of the data.
# 'n' is the number of values to be read.

    if (class(obj) != 'xdr') stop('expect a parameter of class "xdr"')

    unitsize <- xdr.type.size(type)

    seek(obj$fid, addr)
    readBin(obj$fid, what = type, n = n, size = unitsize, endian = obj$endian)
}


# Read in a list of values of the same type.
# The different from 'xdr.array.read' is that the numbers
# are not continguous in the file.
xdr.readList <-
function(obj, addr, type) {
# 'obj' is of class 'xdr'.
# 'addr' is a vector of the addresses in the file of a list of values
#   to retrieve. The very begining of the file has 'addr' value 0.

    if (class(obj) != 'xdr') stop('expect a parameter of class "xdr"')

    x <- numeric(length(addr))
    unitsize <- xdr.type.size(type)

    for (i in 1 : length(addr)) {
        seek(obj$fid, addr[i])
        x[i] <- readBin(obj$fid, what = type, n = 1, size = unitsize,
            endian = obj$endian)
    }

    dim(x) <- dim(addr)
    return(x)
}


# Read in a single number.
xdr.read <-
function(obj, addr, type) {
    xdr.readArray(obj, addr, type, 1)
}


xdr.type.size <-
function(type) {

    if (type == 'double') {
        return(4)
    } else if (type == 'long') {
        return(4)
    } else {
        stop(paste('type "', type,
            '" unknown or not yet implemented', sep = ''))
    }
}


print.xdr <-
function(x) {
# This is a generic function.
# Usage: print(x)
#    where 'x' is of class 'xdr'.

    cat(' file:                  ', x$file, '\n')
    cat(' class:                 ', class(x), '\n')
    cat(' size:                  ', x$size, ' bytes\n')
}


close.xdr <-
function(x) {
    close(x$fid)
}
