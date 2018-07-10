#'
#'
#' @export
#'
table_endmembers <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  data <- signatures$data

  endmembers <- signatures$endmembers
  endNames <- signatures$endNames
  files <- signatures$files
  cbind(endmember = endNames, file = files[endmembers])
}



#' Generate the table which contains the composition of each signature expressed as a weighted combination of endmembers.
#'
#' @export
#' @import dplyr
#' @import knitr
#' @param signatures A spectral object built using the load_files function
#'
#' @seealso \code{\link{clustering}}
#' @seealso \code{\link{set_endmembers}}
#' @seealso \code{\link{load_files}}
#'
table_weights <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  data <- cbind(cluster = clusters, signatures$data)
  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)

  weightsRaw <- .compute_weights(signatures)
  weights <- cbind(file = row.names(signatures$data), cluster = clusters, weightsRaw)

  residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
    original <- x[ 1:length(signatures$range) ]
    weights  <- x[ (length(signatures$range)+1):length(x) ]
    .get_residuals(original, weights, emData[,signatures$range])
  })

  ordering <- dplyr::arrange(cbind(weights, residual=residuals), cluster, file)$file
  cbind(weights, residual=residuals)[match(ordering, weights$file),]
}

#' Represents each signature as a weighted combination of endmembers.
#' This function generates a table which contains the residuals of this adjustment: the sum of the errors between the represented and the real values.
#'
#' @export
#' @import knitr
#' @param signatures A spectral object built using the load_files function
#'
#' @seealso \code{\link{clustering}}
#' @seealso \code{\link{set_endmembers}}
#' @seealso \code{\link{load_files}}
#'
table_residuals_summary <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  data <- cbind(cluster = clusters, signatures$data)
  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)

  weightsRaw <- .compute_weights(signatures)
  weights <- cbind(file = row.names(signatures$data), cluster = clusters, weightsRaw)

  residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
    original <- x[ 1:length(signatures$range) ]
    weights  <- x[ (length(signatures$range)+1):length(x) ]
    .get_residuals(original, weights, emData[,signatures$range])
  })

  summary(residuals)
}
