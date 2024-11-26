#' myci
#'
#' @param sample a sample
#'
#' @return confidence intervals of 95%
#' @export
#'
#' @examples
myci = function (sample) {
  n = length(sample)
  t_alpha_2 = qt(1-.05/2, n-1)
  mp = c(-1, 1)

  mean(sample) + mp * t_alpha_2 * (sd(sample)/sqrt(n))
}
