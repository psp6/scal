#' Calculates sample size for estimating a mean
#'
#' The function requires 1) desired precision 2) expected variability in outcome and 3)
#' level of confidence as required input.
#'
#' @param \sigma expected variability of the outcome
#' @param d precision or margin of the error for the outcome
#' @param alpha level for the confidence interval
#'
#' @return output Sample size
#' @keywords means, , precision
#' @export
#' @examples
#' scal_g1m(10, 3, 0.05)
#'
#'


scal_g1m<- function(sigma, d, alpha){
  z = qnorm(1-alpha/2)
  n = (z^2 * sigma^2)/d^2
  round(n)
  }

