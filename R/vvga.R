#' Vector Natural Visibility Graph
#'
#' This procedure uses time series vector and convert it
#' into network called temporal network
#'
#' @docType package
#'
#' @note This procedure returns diagonal elements of adjacency matrix as connected (1).
#'
#' @param v A multi-dimensional vector.
#'
#' @usage vvga(v)
#'
#' @keywords NVG, 'Visibility Graphs', Graphs, 'Complex Network'
#'
#' @examples
#' \dontrun{
#' data(chaosdata)
#' v<-fnn_algo(chaosdata$Lorenz,tau=3,mmax=10)
#' vvga(v)
#' }
#'
'vvga'
