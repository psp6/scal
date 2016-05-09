#' Calculates sample size for estimating a proportion
#'
#' The function requires 1) assummed proportion 2) precision and 3)
#' level of confidence as required input.
#'
#' @param p expected or assummed proportion of the outcome
#' @param d precision or margin of the error for the outcome
#' @param alpha level for the confidence interval
#'
#' @return Sample size
#' @keywords proportions, prevalence, precision
#' @export
#' @examples
#'
#' scal_g1p(p=0.5, d=0.05, alpha=0.05)


scal_g1p <- function(p, d, alpha){
  z <- qnorm(1-alpha/2)
  n <- (z^2 * p * (1-p))/d^2
  round(n)
}
