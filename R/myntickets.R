#' @title Airplane ticket problem function
#'
#' @param N Number of seats, default 200
#' @param gamma Probability each passenger is a no-show, default 0.02
#' @param p Desired probability that N or fewer passengers show up, default 0.95
#'
#' @return Produces graphs for the objective function vs n for both the binomial calculation and the normal distribution. Also prints a named list of nd, nc, N, p, and gamma.
#' @export
#'
#' @examples
#' \dontrun{myntickets()}
myntickets = function(N = 200, gamma = 0.02, p = 0.95){
  n <- seq(N, floor(N + N/10), by = 1)

  # Binomial
  tempn = qbinom(p = 1-gamma, size = n, prob = p)
  tempn_len = length(tempn)
  for (i in 1:tempn_len) {
    if (tempn[i] >= N) {
      nd = tempn[i] + i
      break
    }
  }
  tmp <- 1 - gamma - pbinom(q = N, size = n, prob = p)
  ind <- which.min(abs(tmp))
  plot(x = n, y = tmp, pch = 19, type = 'b', lty = 5)
  abline(v = nd, col = 'Red')
  abline(a = abs(tmp[ind]), b = 0, col = 'Red')


  # Normal approximation
  n <- seq(N, floor(N + N/10), by = 1)

  tempn = qnorm(1-gamma, n*p, (n*p*(1-p))^(0.5)) + 0.5
  tempn_len = length(tempn)
  for (i in 1:tempn_len) {
    if (tempn[i] >= N) {
      nc = tempn[i] + i - 1
      break
    }
  }


  tmp2 <- 1 - gamma - pnorm(q = N, mean = n*p, sd = (n*p*(1-p))^(0.5))
  ind2 <- which.min(abs(tmp2))
  curve(1 - gamma - pnorm(q = N, mean = x*p, sd = (x*p*(1-p))^(0.5)) , xlim = c(N, N + N/10))
  abline(v = nc, col = 'Blue')
  abline(a = abs(tmp2[ind2]), b = 0, col = 'Blue')

  namedlist <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(namedlist)
}
