# This is a function named 'scal_g1p'
# which calculates sample size for estimating proportions.
#

scal_g1p <- function(p, d, alpha){
  z <- qnorm(1-alpha/2)
  n <- (z^2 * p * (1-p))/d^2
  n
}
