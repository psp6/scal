#' Calculates sample size for comparing two proportions
#'
#' The function requires 1) expected prop in two groups 2) desired significance level 3)
#' level of power as required input.
#'
#' @param \sigma expected variability of the outcome
#' @param d precision or margin of the error for the outcome
#' @param alpha level for the confidence interval
#'
#' @return output Sample size
#' @keywords proportions
#' @export
#' @examples
#' scal_g2p(0.10, 0.30, 0.05, 0.80)
#'
#'

scal_g2p <- function(p1, p2, alpha, power){
  z1 <- qnorm(1-alpha/2)
  z2 <- qnorm(power)

  n <- ((z1+z2)^2 * (p1*(1-p1) + p2*(1-p2)))/(p1-p2)^2
  cat("Sample size in each group", round(n))
}
