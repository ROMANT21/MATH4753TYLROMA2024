#' The get mode function
#'
#' @param v a vector of quantitative data
#'
#' @return the mode of the vector
#' @export
#'
#' @examples
#' getmode(c(1, 2, 3, 3, 5))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
