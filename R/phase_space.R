#' Natural Visibility Graph
#'
#' This procedure uses time series data and convert it
#' into vector/multi-variate time series data
#'
#' @docType package
#'
#' @note This procedure returns diagonal elements of adjacency matrix as connected (1).
#' @param x A numeric vector.
#' @param m Embedding dimension.
#' @param tau Time delay.
#' @usage phase_space(x,m,tau)
#'
#' @keywords Phase Space, 'Chaos'
#'
#' @examples
#' phase_space(1:10,3,1)
#'
#'
'phase_space'
