


## Density of scaled inverse chi-square distribution.
dinvchisq <- function(x, df, scale)
# Both 'df' and 'scale' are single values.
{
    df.half <- df/2
    (df.half * scale)^df.half / gamma(df.half) *
        exp(-df.half * scale / x) / x^(1 + df.half)
}


# Quantile function of scaled inverse chi-square.
qinvchisq <- function(p, df, scale)
# 'df * scale / x' is chisq.
{
    df * scale / qchisq(1 - p, df)
}



## Draw a random sample from scaled inverse chi-square distribution.
rinvchisq <- function(n, df, scale)
# Both 'df' and 's2' are single values.
{
    df * scale / rchisq(n, df)
}
