#' Square Each Element of a Numeric Vector (Rcpp Version)
#'
#' This function squares each element of a numeric vector using Rcpp.
#'
#' @param data A numeric vector.
#' @param start_p Starting point of the data (after this point predictions start).
#' @param m Embedding dimension.
#' @param neighbors Number of neighbors to consider.
#'
#' @return A forecast of the numeric vector.
#' @usage local_approx(data, start_p, m, neighbors)
#'
#' @examples#'
#' \dontrun{
#'   local_approx(data = 1:10, start_p = 2, m = 2, neighbors = 1)
#' }
#' @export
"local_approx"
