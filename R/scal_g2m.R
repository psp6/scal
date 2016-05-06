#' Calculates sample size for comparing two means
#'
#' The function requires 1) expected means & sds in two groups 2) desired significance level 3)
#' level of power as required input.
#'
#' @param \mu_{1} expected mean in group1
#' @param \mu_{2} expected mean in group2
#' @param \sd_{1} expected sd in group1
#' @param \sd_{2} expected sd in group2
#' @param power desired level of power
#' @param alpha desired level of significance
#'
#' @return output Sample size
#' @keywords means,two groups, power
#'
#' @examples
#' scal_g2m(50, 70, 20, 20, 0.05, 0.80)
#'
#'

scal_g2m <- function(mu1, mu2, sd1, sd2, alpha, power){

  z1 <- qnorm(1-alpha/2)
  z2 <- qnorm(power)

  sd <- (sd1 + sd2)/2

  d <- (mu1 - mu2)

  n <- ((z1+z2)^2 * (sd)^2) /d ^2

  cat("Sample size in each group =", round(n))
}
