#' @title Normal Curve Generator and Lower Tail calculator
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a quantile
#'
#' @return Produces a curve with the lower tail shaded and returns the calculated lower tail probability
#' @export
#'
#' @examples
#' \dontrun{myncurve(0,2,1)}
myncurve = function(mu, sigma, a){
  pseudoinf = mu - (sigma * 15)
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(pseudoinf,a,length=10000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(pseudoinf,xcurve,a),c(0,ycurve,0),col="Red")
  pnorm(a,mu,sigma)
}
