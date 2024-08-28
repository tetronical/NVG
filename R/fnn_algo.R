#' False Nearest Neighbour
#'
#' False Nearest Neighbour is a dimensionality estimation method.
#' This method uses a delay value to reconstruct phase space and identifies the dimension
#' based on true neighbors. Examples include Rossler system, Lorenz Data, and Mackey and Glass data.
#'
#' @docType package
#'
#' @keywords FNN, Dimension, Phase-Space
#'
#' @param x A numeric vector or matrix.
#' @param tau An integer specifying the time delay. Default is 1.
#' @param mmax An integer specifying the maximum embedding dimension. Default is 10.
#' @param rtol A numeric value for the relative tolerance. Default is 15.
#' @param atol A numeric value for the absolute tolerance. Default is 2.
#' @param plot Logical, whether to plot the results. Default is FALSE.
#'
#' @return A matrix of False Nearest Neighbour values.
#'
#' @usage fnn_algo(x, tau = 1,  mmax = 10, rtol = 15, atol = 2, plot = FALSE)
#'
#' @examples
#' \dontrun{
#' data(chaosdata)
#' fnn_algo(x, tau = 1, mmax = 10, rtol = 15, atol = 2, plot = FALSE)
#' }
#' @importFrom stats acf sd
#' @importFrom tseriesChaos mutual
#' @export
#'
#'
fnn_algo <- function(x, tau = 1, mmax = 10, rtol = 15, atol = 2, plot = FALSE) {

  AMI_func <- function(x) {
    # Compute Average Mutual Information
    lagged_data <- tseriesChaos::mutual(x, lag.max = length(x),
                                        partitions = length(x), plot = FALSE)
    lag_position <- diff(lagged_data, 1)
    j <- which(lag_position > 0)[1] - 1 # -1 is for lag 0
    return(j)
  }

  tau <- ifelse(tau == 'ACF',
                (which(stats::acf(x, length(x), plot = FALSE)$acf <= 0)[1] - 1),
                ifelse(tau == 'AMI', AMI_func(x), tau))

  N <- length(x)
  Ra <- stats::sd(x)
  FNN <- matrix(data = NA, nrow = mmax)

  psr_deneme <- function(x, m, tau, npoint) {
    M <- npoint
    Y <- matrix(data = 0, M, m)
    for (i in 1:m) {
      Y[, i] <- t(x[(1:M) + (i - 1) * tau])
    }
    return(Y)
  }

  for (m in 1:mmax) {
    M <- N - (m * tau)
    Y <- psr_deneme(x, m, tau, M)
    FNN[m, 1] <- 0
    for (n in 1:M) {
      Y0 <- matrix(rep(Y[n, ], each = M), nrow = M)
      distance <- sqrt(rowSums((Y - Y0)^2))
      near <- sort(distance, index.return = TRUE)
      names(near) <- c("distance", "position")
      near_position_index <- which(near$distance != 0)[1]
      D <- abs(x[n + m * tau] - x[near$position[near_position_index] + m * tau])
      R <- sqrt(D^2 + near$distance[near_position_index]^2)
      if (((D / near$distance[near_position_index]) > rtol) || (R / Ra > atol)) {
        FNN[m, 1] <- FNN[m, 1] + 1
      }
    }
  }

  FNN <- (FNN / FNN[1, 1]) * 100

  if (plot) {
    plot(1:mmax, FNN, type = "p", ylab = "% FNN", xlab = "Embedding Dimension",
         main = "False Nearest Neighbour")
  }

  return(FNN)
}
