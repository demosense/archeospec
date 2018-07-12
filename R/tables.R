#' Endmembers description
#'
#' Generate the table which contains the endmembers and the files which they corresponds to.
#'
#' @export
#' @import dplyr
#' @import knitr
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#' @return A matrix object with the tabular information.
#'
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' unmixed_signatures <- unmixing_vca(signatures, k=2)
#' table_endmembers(unmixed_signatures)
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


#' Signature endmember weights
#'
#' Generate the table which contains the composition of each signature expressed as a weighted combination of endmembers.
#' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#'
#' @export
#' @import dplyr
#' @import knitr
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#' @return A matrix object with the tabular information.
#'
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' unmixed_signatures <- unmixing_fixed(signatures, files=c("almagre.asd.txt", "blanco.asd.txt"))
#' table_weights(unmixed_signatures)
#'
table_weights <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  data <- signatures$data
  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)

  weightsRaw <- .compute_weights(signatures)
  weights <- cbind(file = row.names(signatures$data), weightsRaw)

  residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
    original <- x[ 1:length(signatures$range) ]
    weights  <- x[ (length(signatures$range)+1):length(x) ]
    .get_residuals(original, weights, emData[,signatures$range])
  })

  ordering <- dplyr::arrange(cbind(weights, residual=residuals), file)$file
  cbind(weights, residual=residuals)[match(ordering, weights$file),]
}

#' Residuals for each signature
#'
#' Represents each signature as a weighted combination of endmembers.
#' This function generates a table which contains the residuals of this adjustment: the sum of the errors between the represented and the real values.
#'
#' @export
#' @import knitr
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#' @return A matrix object with the tabular information.
#'
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' unmixed_signatures <- unmixing_fixed(signatures, files=c("almagre.asd.txt", "blanco.asd.txt"))
#' table_residuals_summary(unmixed_signatures)
#'
table_residuals_summary <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  data <- signatures$data
  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)

  weightsRaw <- .compute_weights(signatures)
  weights <- cbind(file = row.names(signatures$data), weightsRaw)

  residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
    original <- x[ 1:length(signatures$range) ]
    weights  <- x[ (length(signatures$range)+1):length(x) ]
    .get_residuals(original, weights, emData[,signatures$range])
  })

  t(as.matrix(summary(residuals)))
}
