#' Parallel execution.
#'
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom parallel clusterExport clusterCall clusterSetRNGStream parLapply
do.parallel <- function(
    FUN,
    x,
        # List or vector, analogous to the first argument to
        # 'lapply'. Each element of 'x' is the first argument
        # to one call of 'FUN'.
    ...,
        # Additional arguments passed on to 'FUN'.
    pkgs = NULL,
        # Character vector naming packages to be loaded
        # on the worker nodes.
    export = NULL,
        # Character vector naming variables in the global env to be
        # exported to the workers.
    n.cores = parallel::detectCores()
    )
{
    if (is.logical(n.cores))
    {
        if (n.cores)
        {
            n.cores <- parallel::detectCores()
            if (is.na(n.cores))
                n.cores <- 0L
            else
                if (n.cores > 4) n.cores <- n.cores - 2L
        } else
            n.cores <- 0L
    } else
    {
        stopifnot(is.numeric(n.cores), trunc(n.cores) == n.cores)
    }
    n.cores <- min(n.cores, length(x))

    if (n.cores > 1L)
    {
        # Parallel code seems to be at odds
        # with 'system.time' and 'Rprof'.

        # The environment of the function called by 'parLapply'
        # will be carried along with the function
        # body over to all the workers.
        # Therefore, the function should not be defined in a
        # environment that contains many large objects.

        my.seed <- trunc(runif(1) * 10000)

        i.repeat <- 0L
        max.repeat <- 3L
        repeat
        {
            if (i.repeat == max.repeat)
            {
                warning('Parallel code failed. ',
                    'Switching to serial code to finish this task...')
                break
            } else
            {
                if (i.repeat > 0L)
                    warning('Parallel code failed. Trying again...')
                i.repeat <- i.repeat + 1L
            }

#           cl <- tryCatch(makeCluster(n.cores, type = ifelse(
#                   .Platform$OS.type == 'unix', 'FORK', 'PSOCK')),
#                   error = function(...) NULL)
            cl <- tryCatch(parallel::makeCluster(n.cores),
                    error = function(...) NULL)
            if (is.null(cl))
                next
            else
                on.exit(parallel::stopCluster(cl))

                # FIXME:
                # Is 'mclapply' faster on *nix systems?
                # My experimentations appeared to say NO.


            if (!is.null(export))
                parallel::clusterExport(cl, export)

            if (!is.null(pkgs))
                parallel::clusterCall(cl,
                    function(pkgs) {
                        for (name in pkgs)
                            library(name, character.only = TRUE)
                        invisible(NULL)
                    },
                    pkgs = pkgs)

            rng.kind <- RNGkind()
            on.exit(RNGkind(rng.kind[1], rng.kind[2]), add = TRUE)
            RNGkind("L'Ecuyer-CMRG")
            parallel::clusterSetRNGStream(cl, my.seed)
                # FIXME: not sure this is the correct use.

            z <- tryCatch(parallel::parLapply(cl, x, FUN, ...),
                    error = function(...) NULL)
                        # Prefer 'parLappluy' to 'clusterApply'
                        # and 'clusterApplyLB'.
                        # FIXME
                        # Error in here is not necessarily due to
                        # parallelism; it could be a failure of
                        # 'FUN'. Ideally, one should make 'FUN'
                        # always successfully return so that
                        # there is no confusion as to what caused
                        # the preceding line to fail.
            if (is.null(z))
            {
                parallel::stopCluster(cl)
                RNGkind(rng.kind[1], rng.kind[2])
                on.exit()
                next
            } else
                return(z)
        }
    }

    lapply(x, FUN, ...)
        # Run if no parallel or parallel has failed.
}


