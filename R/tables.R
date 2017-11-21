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
  .check_endmembers_clusters(signatures)

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers

  lapply(
    1:length(clusters),
    function(i) {

      classes <- clusters[[i]]
      ems <- endmembers[[i]]
      data <- cbind(class = classes, signatures$data)
      k <- length(ems)
      emData <- data[ems,]
      endNames <- paste0("end", 1:length(endmembers[[i]]))
      emData <- cbind(endmember=endNames, emData)

      weightsRaw <- .compute_weights(signatures, i)
      weights <- cbind(file = row.names(signatures$data), class = clusters[[i]], weightsRaw)

      residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
        original <- x[ 1:length(signatures$range) ]
        weights  <- x[ (length(signatures$range)+1):length(x) ]
        .get_residuals(original, weights, emData[,signatures$range])
      })

      ordering <- dplyr::arrange(cbind(weights, residual=residuals), class, file)$file
      cbind(weights, residual=residuals)[match(ordering, weights$file),]
    }
  )
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
table_residuals <- function(signatures) {

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers

  lapply(
    1:length(endmembers),
    function(i) {

      classes <- clusters[[i]]
      ems <- endmembers[[i]]
      data <- cbind(class = classes, signatures$data)
      k <- length(ems)
      emData <- data[ems,]
      endNames <- paste0("end", 1:length(endmembers[[i]]))
      emData <- cbind(endmember=endNames, emData)

      weightsRaw <- .compute_weights(signatures, i)
      weights <- cbind(file = row.names(signatures$data), class = clusters[[i]], weightsRaw)

      residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
        original <- x[ 1:length(signatures$range) ]
        weights  <- x[ (length(signatures$range)+1):length(x) ]
        .get_residuals(original, weights, emData[,signatures$range])
      })

      summary(residuals)
    }
  )
}
