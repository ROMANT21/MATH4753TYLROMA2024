# N: number of seats sold
# p: probability of a show
# gamma: probability that the flight will be overbooked
# return: the number of tickets to be sold
#' ntickets
#'
#' @param N number of seats sold
#' @param gamma probability that the flightwill be overbooked
#' @param p probability of a show
#'
#' @return a list of tickets to be sold using discrete distribution,
#'          tickets to be sold using normal distribution, N, p, and gamma.
#' @importFrom graphics layout lines abline
#' @importFrom stats pbinom
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets = function(N, gamma, p) {
  # Create layout for plots
  layout(matrix(c(1, 2), 2, 1, byrow=TRUE))

  n <- seq(N, floor(N+N/10), by=1)
  floor(N+N/10)
  # Discrete Distribution
  tmp <- 1 - gamma - pbinom(q=N,
                            size = n,
                            prob = p)
  ind <- which.min(abs(tmp))
  min_num_seats = n[ind]

  main_label = paste0("Objective Vs n to find optimal tickets sold\n (",
                      min_num_seats, ") gamma=", gamma, " N=", N, " discrete")
  plot(n, tmp, main=main_label, ylab="Objective", xlab="n")
  lines(n, tmp, main=main_label)
  abline(h=tmp[ind])
  abline(v=min_num_seats)

  # Normal Approximation
  norm_func = function(x) {
    1 - gamma - pnorm(q = N + 0.5,
                      mean = (x * p),
                      sd = sqrt(x * p * (1 - p))
    )
  }

  nc = stats::uniroot(norm_func, interval=c(N, N+N/10), tol=1e-12)$root
  main_label = paste0("Objective Vs n to find optimal tickets sold\n (",
                      nc, ") gamma=", gamma, " N=", N, " discrete")
  curve(norm_func(x), from=N, to=N+N/10,
        main=main_label, ylab="Objective", xlab="n")
  abline(v=nc)
  abline(h=norm_func(nc))

  list(nd=n[ind], nc=nc, N=N, p=p, gamma=gamma)
}
