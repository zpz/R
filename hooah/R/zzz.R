.First.lib <-
function(...) {

    options(papersize = 'letter')

    ps.options(onefile = FALSE, horizontal = FALSE)

}


.onLoad <- function(libname, pkgname)
{
    do.call(setOptions, .defaultOptions())
    options(error = utils.recover)
}

