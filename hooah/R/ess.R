
## Effective sample size.
## Reference:
## Kong, Liu, and Wong, 1994,
## "Sequential imputations and Bayesian missing data problems",
## JASA, 89(425).
ess <- function(weight)
{
    m <- mean(weight)
    cv <- mean((weight - m)^2) / (m * m)
    length(weight) / (1 + cv)
}
