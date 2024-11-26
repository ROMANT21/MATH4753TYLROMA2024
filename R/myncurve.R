#' A normal curve function that visualizes probability
#'
#' @param mu the mean of the distribution
#' @param sigma the standard devaition of the distribution
#' @param a the probability to find for the distribution
#'
#' @return A list with mu, sigma, and area
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @examples
#' myncurve(5, 2, 2)
myncurve = function(mu, sigma, a){

  # Plot the curve
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),
        ylab="Normal density", xlab="Y")

  # Shade the region for the probability
  xcurve=seq(-20, a, length=1000)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(-20,xcurve,a),c(0, ycurve,0),col="Red")


  # Calculate the probability
  area=pnorm(a, mean=mu, sd=sigma)
  area=round(area,4)

  return(list(mu = mu, sigma = sigma, area=area))
}
