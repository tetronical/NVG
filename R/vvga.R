#' Vector Natural Visibility Graph
#'
#' This procedure uses time series vector and convert it
#' into network called temporal network
#'
#' @docType package
#'
#' @note This procedure returns diagonal elements of adjacency matrix as connected (1).
#'
#' @usage vvga(m)
#' m is a multi-dimensional vector
#'
#' @keywords NVG, 'Visibility Graphs', Graphs, 'Complex Network'
#'
#' @examples
#' data(chaosdata)
#' m<-fnn_algo(chaosdata$Lorenz,tau=3,mmax=10)
#' vvga(m)
#'
#'
'vvga'
