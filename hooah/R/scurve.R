
## A S-shaped curve for positive values:
##   1 - (1 + a) / (1 + a * exp(b * x))
## where 'a' controls the curvature and
## 'b' controls how fast the curve reaches 1.
## 'a' in [0.005, 0.05] gives a pretty nice shape.
scurve <- function(
    x,
    a = 0.05,
    half = (min(x) + max(x)) / 2
        # Where the curve should take value 0.5?
    )
{
    # Let (1 + a) / (1 + a * exp(b * half)) = 1/2
    # ==>
    #   b = log(1/a + 2) / half
    b <- log(1/a + 2) / half

    1 - (1 + a) / (1 + a * exp(b * x))
}


